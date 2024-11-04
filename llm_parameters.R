library(knitr)

################################################################
# @brief 计算大模型的参数规模，
# 输入参数具体数值：
#   model_name: 模型的名称
#   n_vocabulary: 模型的 Token 总量
#   n_ctx: 模型的窗口大小
#   d_model: 模型的 embedding 向量维度
#   d_q: W^Q 矩阵的行向量维度，一般等于 d_head
#   d_k: W^K 矩阵的行向量维度，一般等于 d_head
#   d_v: W^V 矩阵的行向量维度，一般等于 d_head
#   n_heads: 每一个解码器中多头注意力矩阵的头数
#   n_layers: 解码器的层数
#   d_ff: 前向反馈网络隐藏层的维度
################################################################

# 每个解码器中的注意力机制参数大小
# multi-head-attention = concat(Z_1,...,Z_n_heads)W^O
single_decoder_attention_cnt <- function(
  d_model,        # 模型的 embedding 向量维度
  d_q,            # W^Q 矩阵的行向量维度
  d_k,            # W^K 矩阵的行向量维度
  d_v,            # W^V 矩阵的行向量维度
  n_heads         # 每一个解码器中多头注意力矩阵的头数
) {
  # W^O 矩阵的大小
  n_output      <- n_heads * d_v * d_model

  n_single_head <- d_model * d_q + d_model * d_k + d_model * d_v
  n_multi_head  <- n_heads * n_single_head + n_output

  return(n_multi_head)
}

# 每个解码器中的前向反馈网络的参数大小
# FFN(x) = max(0, xW_1 + b_1)W_2 + b_2
# W_1: d_model x d_ff, b_1: d_ff
# W_2: d_ff x d_model, b_2: d_model
single_decoder_ffn_cnt <- function(
  d_model,         # 模型的 embedding 向量维度
  d_ff             # 前向反馈网络隐藏层的维度
) {
  n_ffn <- d_model * d_ff + d_ff * d_model + d_ff + d_model
  return(n_ffn)
}

# 每个解码器中的归一化层的参数大小
# 每个解码器都有两个归一化层，且每个归一化层需要 2 * d_model 个参数（一个用于缩放，一个用于偏移）
single_decoder_norm_cnt <- function(
  d_model         # 模型的 embedding 向量维度
) {
  n_norm <- d_model * 2 * 2
  return(n_norm)
}

# 每个解码器的参数大小
single_decoder_cnt <- function(
  d_model,         # 模型的 embedding 向量维度
  d_q,             # W^Q 矩阵的行向量维度
  d_k,             # W^K 矩阵的行向量维度
  d_v,             # W^V 矩阵的行向量维度
  n_heads,         # 每一个解码器中多头注意力矩阵的头数
  d_ff             # 前向反馈网络隐藏层的维度
) {
  n_attention  <- single_decoder_attention_cnt(d_model,
                                               d_q,
                                               d_k,
                                               d_v,
                                               n_heads)
  n_ffn        <- single_decoder_ffn_cnt(d_model, d_ff)
  n_norm       <- single_decoder_norm_cnt(d_model)
  n_decoder    <- n_attention + n_ffn + n_norm

  return(n_decoder)
}

get_llm_parameters_cnt <- function(
  model_name = "",
  n_vocabulary,    # 模型的总 token 量
  n_ctx,           # 模型的 prompt 窗口大小
  d_model,         # 模型的 embedding 向量维度
  d_q,             # W^Q 矩阵的行向量维度
  d_k,             # W^K 矩阵的行向量维度
  d_v,             # W^V 矩阵的行向量维度
  n_heads,         # 每一个解码器中多头注意力矩阵的头数
  n_layers,        # 解码器的层数
  d_ff             # 前向反馈网络隐藏层的维度
) {
  n_embedding  <- n_vocabulary * d_model
  n_position   <- n_ctx * d_model

  # n_layers 层解码器的总参数
  n_attention  <- n_layers * single_decoder_attention_cnt(d_model,
                                                          d_q,
                                                          d_k,
                                                          d_v,
                                                          n_heads)
  n_ffn        <- n_layers * single_decoder_ffn_cnt(d_model, d_ff)
  n_norm       <- n_layers * single_decoder_norm_cnt(d_model)
  n_top_norm   <- 2 * d_model # 最顶层的 Norm 层的参数

  total_params <- n_embedding + n_position + n_attention + n_ffn + n_norm +
    n_top_norm

  # 输出相关参数
  df <- data.frame(Model     = model_name,
                   Embedding = n_embedding,
                   Emd_Rate  = round(n_embedding / total_params, 1),
                   Position  = n_position,
                   Attention = n_attention,
                   Atn_Rate  = round(n_attention / total_params, 1),
                   Ffn       = n_ffn,
                   Ffn_Rate  = round(n_ffn / total_params, 1),
                   Norm      = n_norm,
                   Total     = total_params)
  print(kable(df, format = "markdown"))

  return(total_params)
}

get_llm_list_parameters_cnt <- function(llm_list) {
  llm_params_list <- list()

  for (l in llm_list) {
    n_embedding  <- l$n_vocabulary * l$d_model
    n_position   <- l$n_ctx * l$d_model

    # n_layers 层解码器的总参数
    n_attention  <- l$n_layers * single_decoder_attention_cnt(l$d_model,
                                                              l$d_q,
                                                              l$d_k,
                                                              l$d_v,
                                                              l$n_heads)
    n_ffn        <- l$n_layers * single_decoder_ffn_cnt(l$d_model, l$d_ff)
    n_norm       <- l$n_layers * single_decoder_norm_cnt(l$d_model)
    n_top_norm   <- 2 * l$d_model
    total_params <- n_embedding + n_position + n_attention + n_ffn + n_norm +
      n_top_norm
    # 输出相关参数
    df <- data.frame(Model     = l$model_name,
                     Embedding = n_embedding,
                     Emd_Rate  = round(n_embedding / total_params, 1),
                     Position  = n_position,
                     Attention = n_attention,
                     Atn_Rate  = round(n_attention / total_params, 1),
                     Ffn       = n_ffn,
                     Ffn_Rate  = round(n_ffn / total_params, 1),
                     Norm      = n_norm,
                     Total     = total_params)
    llm_params_list <- append(llm_params_list, list(df))
  }

  final_df <- do.call(rbind, llm_params_list)
  print(kable(final_df, format = "markdown"))
}

llm_list <- list(
  list(model_name = "GPT-1",
       n_vocabulary = 40000,
       n_ctx = 512,
       d_model = 768,
       d_q = 64,
       d_k = 64,
       d_v = 64,
       n_heads = 12,
       n_layers = 12,
       d_ff = 768 * 4),
  list(model_name = "GPT-2-Small",
       n_vocabulary = 50257,
       n_ctx = 1024,
       d_model = 768,
       d_q = 64,
       d_k = 64,
       d_v = 64,
       n_heads = 12,
       n_layers = 12,
       d_ff = 768 * 4),
  list(model_name = "GPT-3-Small",
       n_vocabulary = 50257,
       n_ctx = 2048,
       d_model = 768,
       d_q = 64,
       d_k = 64,
       d_v = 64,
       n_heads = 12,
       n_layers = 12,
       d_ff = 768 * 4),
  list(model_name = "GPT-3-Medium",
       n_vocabulary = 50257,
       n_ctx = 2048,
       d_model = 1024,
       d_q = 64,
       d_k = 64,
       d_v = 64,
       n_heads = 16,
       n_layers = 24,
       d_ff = 1024 * 4),
  list(model_name = "GPT-3-Large",
       n_vocabulary = 50257,
       n_ctx = 2048,
       d_model = 1536,
       d_q = 96,
       d_k = 96,
       d_v = 96,
       n_heads = 16,
       n_layers = 24,
       d_ff = 1536 * 4),
  list(model_name = "GPT-3-XL",
       n_vocabulary = 50257,
       n_ctx = 2048,
       d_model = 2048,
       d_q = 128,
       d_k = 128,
       d_v = 128,
       n_heads = 24,
       n_layers = 24,
       d_ff = 2048 * 4),
  list(model_name = "GPT-3-2.7B",
       n_vocabulary = 50257,
       n_ctx = 2048,
       d_model = 2560,
       d_q = 80,
       d_k = 80,
       d_v = 80,
       n_heads = 32,
       n_layers = 32,
       d_ff = 2560 * 4),
  list(model_name = "GPT-3-6.7B",
       n_vocabulary = 50257,
       n_ctx = 2048,
       d_model = 4096,
       d_q = 128,
       d_k = 128,
       d_v = 128,
       n_heads = 32,
       n_layers = 32,
       d_ff = 4096 * 4),
  list(model_name = "GPT-3-13B",
       n_vocabulary = 50257,
       n_ctx = 2048,
       d_model = 5140,
       d_q = 128,
       d_k = 128,
       d_v = 128,
       n_heads = 40,
       n_layers = 40,
       d_ff = 5140 * 4),
  list(model_name = "GPT-3-175B",
       n_vocabulary = 50257,
       n_ctx = 2048,
       d_model = 12288,
       d_q = 128,
       d_k = 128,
       d_v = 128,
       n_heads = 96,
       n_layers = 96,
       d_ff = 12288 * 4)
)

get_llm_list_parameters_cnt(llm_list)
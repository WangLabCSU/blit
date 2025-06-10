# 加载必需的库
library(stringr)

# 生成minimap2命令函数
minimap2 <- function(fq1 = "fastq1", fq2 = "fastq2", ofile1, ofile2 = NULL, minimap2_path = "minimap2") {
  # 校验输入文件是否存在
  if (!file.exists(fq1)) stop(paste("File", fq1, "does not exist"))
  if (!file.exists(fq2)) stop(paste("File", fq2, "does not exist"))

  # 构建基本的minimap2命令
  cmd <- paste(
    minimap2_path,
    "-a", fq1,  # 对应第一个FASTQ文件
    if (!is.null(fq2)) paste("-b", fq2) else "",  # 第二个文件如果存在，加入-b选项
    "-o", ofile1,  # 输出SAM文件
    if (!is.null(ofile2)) paste("-O", ofile2) else ""  # 如果有第二个输出文件，使用-O
  )

  # 返回命令
  return(str_trim(cmd))  # 去除前后的空格
}

# 调用minimap2函数，生成命令
cmd <- minimap2("fastq1", "fastq2", "output.sam", "second_output.sam")
cat("Minimap2 Command: ", cmd)

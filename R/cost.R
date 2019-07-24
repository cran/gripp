# Cost function to be minimized using the inverse problem solver
#
#' @export
cost <- function(parm)
{
 change_parm(.GlobalEnv$folder_name,.GlobalEnv$file_name,.GlobalEnv$parm_name,.GlobalEnv$line_number,.GlobalEnv$parm_type,.GlobalEnv$parm_vector,parm,.GlobalEnv$attrib_str)
 if(.GlobalEnv$command_folder!='wd')
 {
   if (.GlobalEnv$isitR==TRUE)
   {eval(parse(paste(.GlobalEnv$command_folder,'/',.GlobalEnv$command,sep='')))}
   else{system(parse(paste(.GlobalEnv$command_folder,'/',.GlobalEnv$command,sep='')))}
 }else{
   if (.GlobalEnv$isitR==TRUE)
   {eval(parse(.GlobalEnv$command))}
   else{system(parse(.GlobalEnv$command))}
 }
 if(.GlobalEnv$result_folder!='wd')
  {resultado <- unlist(strsplit(readLines(paste(.GlobalEnv$result_folder,'/',.GlobalEnv$result,sep='')),' '))
  }else{resultado <- unlist(strsplit(readLines(.GlobalEnv$result),' '))}
 if(.GlobalEnv$target_folder!='wd')
  {alvo <- unlist(strsplit(readLines(paste(.GlobalEnv$target_folder,'/',.GlobalEnv$target,sep='')),' '))
  }else{alvo <- unlist(strsplit(readLines(.GlobalEnv$target),' '))}
  custo <- 0
 m <- length(alvo)
 j_cs <- 1
 while(j_cs<=m)
 {
  custo <- custo+(as.numeric(resultado[j_cs])-as.numeric(alvo[j_cs]))^2
  j_cs <- j_cs+1
 }
 return(custo)
}

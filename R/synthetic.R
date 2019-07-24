# Synthetic Experimental Data function
#
#' @export
synthetic <- function(parm,sigma)
{
 n <- length(.GlobalEnv$file_name)
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
 n <- length(resultado)
 erro <- rnorm(n,mean=0,sd=sigma)
 resultado<-as.numeric(resultado)*(1+erro)
 return(resultado)
}



#Par?metros de Entrada
#result: #agrupaci?n obtenida mediante un discriminador
#real: agrupaci?n original
#imprimir: variable que indica si se imprime o no la matriz de confusi?n junto con resultados relacionados
#por defecto es igual a cero, esto significa que solo se devolver? el error de la clasificaci?n sin la matriz de confusi?n.



mc <- function(result, real, imprimir = 0)
{


 #para hacer una matriz de confusi?n
 conf<-vector(mode="numeric")
 s=0
 for (i in 1:length(real))
 {
  if (real[i]!=result[i])
  {
    s=s+1
    conf[s]=real[i] #mal clasificados # 1=> bueno clasificado como malo, y 0 viceversa          
  }                                                       
 }

 #total de Buenos en la realidad
 ng<-length(real[real==1])
 #total de malos en la realidad
 nb<-length(real[real==0])
 #total de buenos clasificados como malos
 bg<-length(conf[conf==1])
 #total de malos clasificados como buenos
 gb<-length(conf[conf==0])
 #total de buenos bien clasificados
 gg<-ng-bg
 #total de malos bien clasificados
 bb<-nb-gb

 #TASA DE EXACTITUD
 tasaexact  <- (gg+bb)/(gg+bb+gb+bg) #que tan buena fue la clasificaci?n, ACCURACITY
 precision  <- gg/(gb+gg)
 TP         <- gg/(gg+bg)
 FP         <- gb/(gb+bb)
 TN         <- bb/(bb+gb)
 FN         <- bg/(bg+gg)
 recall     <- gg/(gg + bg) #TP/(TP + FN)
 f1         <- 2*precision*recall/(precision + recall) #2*precision2*recall/(precision2 + recall)


 if (imprimir==1){
   #ORIGINAL
  #l=c("G","B","TOTAL")
  #confusi<-matrix(c(gg,gb,gg+gb,bg,bb,bg+bb,ng,nb,length(real)),ncol=3,nrow=3,byrow = TRUE,
  #                dimnames = list(c("Class", "Predicted",""),c("Class", "Original","")))
  #confusi<-rbind(l,confusi)
  #l=c("","G","B","TOTAL")
  #confusi<-as.data.frame(cbind(l,confusi))
  #print(confusi)
  # TRANSPOSE
  l=c("B","G","TOTAL")
  confusi<-matrix(c(bb,gb,gb+bb, bg,gg,gg+bg,bg+bb,gg+gb,length(real)),ncol=3,nrow=3,byrow = TRUE,
                  dimnames = list(c("Class", "Original",""),c("Class", "Predicted","")))
  confusi <- rbind(l,confusi)
  l=c("","B","G","TOTAL")
  confusi<-as.data.frame(cbind(l,confusi))
  print(confusi)

 print("########################################################") 

 relacionado<-matrix(c(tasaexact,TP, FP, TN, FN, precision, recall, f1),nrow= 8,ncol=1, 
                     dimnames = list(c("E", "TP","FP","TN","FN","P", "recall", "f1")))
 print(relacionado)
 }

 #error de clasificaci?n
 error <- 1 - tasaexact
 TP <- gg/(gg+bg)
 FP <- gb/(gb+bb)
 
 return(list(Error = error, Precision = precision, TP = TP, FP = FP, recall = recall, f1 =f1, 
             pos_cases = gg))
 #return(72000*bg+20000*gb)
 #return(gb)
}
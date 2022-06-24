x=readRDS('data/Donnes_Climat_Exemple_AVIGNON8507.rds')
Precip=ts(x$prec,start=c(2015,1),freq=365)
TAirmean=ts(x$Tair.mean,start=c(2015,1),freq=365)
Windmean=ts(x$wind.mean,start=c(2015,1),freq=365)
HumRelmean=ts(x$humrel.mean,start=c(2015,1),freq=365)
data=cbind(TAirmean,Windmean,HumRelmean,Precip)

library(forecast)

# Cherchons un modèle AR vectoriel
library(vars)
VARselect(data, lag.max=8, type="const",season=365)
var4 <- VAR(data, p=4,type = "const",season = 365,exogen=NULL)
summary(var4)
# la température semble corrélée à toutes les autres variables au temps t-1

# utilisons un modèle tslm pour evaluer la dépendance au temps t
model=tslm(Precip~TAirmean+Windmean+HumRelmean+trend+season,data=data)
summary(model)

# la saisonnalité annuelle avec des données journalières rend difficile l'interpration
# on va la résumer par un effet mensuel
Month=as.numeric(format(as.Date(x$DAY,format="%d/%m/%Y"),"%m"))
data=cbind(data,Month)
model=tslm(Precip~TAirmean+Windmean+HumRelmean+Month,data=data)
summary(model)
# Les précipitations semblent liées à Windmean et HumRelmean au temps t

TAirmeanM1=TAirmean;TAirmeanM1[2:2192]=TAirmean[1:2191]
WindmeanM1=Windmean;WindmeanM1[2:2192]=Windmean[1:2191]
HumRelmeanM1=HumRelmean;HumRelmeanM1[2:2192]=HumRelmean[1:2191]
PrecipM1=Precip;PrecipM1[2:2192]=Precip[1:2191]

data=cbind(TAirmean,WindmeanM1,HumRelmean,Precip,Month,TAirmeanM1,WindmeanM1,HumRelmeanM1,PrecipM1)
model=tslm(Precip~TAirmean+Windmean+HumRelmean+TAirmeanM1+WindmeanM1+HumRelmeanM1+PrecipM1+Month,data=data)
summary(model)
checkresiduals(model)

# Les résidus sont assimilables à un bruit blanc.
# Néanmoins ils n'ont pas l'air gaussien, on aurait du commencer par 
# étudier la distribution de la variable Precip et la transformer
# en échelle logarithmique
# Attention néanmoins il y a des 0 dans les Precip, on pourrait 
# avant de faire la transformation log les remplacer par min(Precip)/10



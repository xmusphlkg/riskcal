
data_for_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, group){
  if(dep_lon == arr_lon & dep_lat == arr_lat){
    inter <- data.frame(
      lon = dep_lon,
      lat = dep_lat,
      group = group
    )
  } else {
    inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
    inter=data.frame(inter)
    inter$group=NA
    diff_of_lon=abs(dep_lon) + abs(arr_lon)
    if(diff_of_lon > 180){
      inter$group[ which(inter$lon>=0)]=paste(group, "A",sep="")
      inter$group[ which(inter$lon<0)]=paste(group, "B",sep="")
    }else{
      inter$group=group
    }
  }
  return(inter)
}
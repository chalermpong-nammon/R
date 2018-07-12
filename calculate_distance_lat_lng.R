# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

#bus <- read.csv("/Users/chalermpongsomdulyawat/Desktop/Grad_workspace/bus_data/bus_with_ridership_1_1_2010.csv", sep = "", quote = "\"'")

bus[1,]$longitude <- 38.7052
bus[1,]$latitude <- -9.16130

lat = 38.7053
lng = -9.16130
for(i in 1:10000){

  bus[1,]$longitude <- lng
  bus[1,]$latitude <- lat
  print(lat)  
  print(lng)
  bus[2,]$longitude <- lng+0.00019
  bus[2,]$latitude <- lat

  distance.km <- gcd.slc(bus[1,]$longitude, bus[1,]$latitude,bus[2,]$longitude, bus[2,]$latitude )
  print (distance.km)
  lat = lat + 0.0001
  
  
}
#38.7051 -9.16134

#find busstop left = 4301 
left = bus[bus$longitude == min(bus$longitude),]

#find busstop rigth = 79107
rigth = bus[bus$longitude == max(bus$longitude),]

#find busstop top = 16003
top = bus[bus$latitude == max(bus$latitude),]

#find busstop low = 19001
low = bus[bus$latitude == min(bus$latitude),]


void assignHeightCell(float x, float y, float people , Date time){
  
  for(int i=0; i<cols; i++){
    for(int j = 0; j<rows; j++){
      if(x < (j+1)*mapScreenWidth/cols && y < (i+1)*mapScreenHeight/rows){
        // println(time.getDate());
        grid[j][i].setHeight(people,findSlotTime(time),time.getDate()-9);
        
        //    print("people :"+people+" time : "+time.getHours());
        return;
      } 
    }
  }
}


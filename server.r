library(shiny)
library(googleVis)

shinyServer(function(input, output) {

output$page1 <- renderGvis({
  m1=gvisGeoChart(mydata, "State", "Sales1",options=list(region="IN", displayMode="regions", resolution="provinces",width=450, height=300,datalessRegionColor="#FFFFFF",
														colorAxis="{values:[3000,4000,5000],colors:['#81BEF7','#5882FA','#013ADF']}"),chartid="m1")
  m2=gvisGeoChart(mydata, "State", "Diff",options=list(region="IN", displayMode="regions", resolution="provinces",width=450, height=300,datalessRegionColor="#FFFFFF",
														colorAxis="{values:[-40,0,40],colors:['#F5BCA9','#FE642E','#8A2908']}"),chartid="m2")
  m3=gvisAnnotatedTimeLine(mydata1, datevar="Date",numvar="Value", idvar="Variable",options=list(displayAnnotations=TRUE,width=900, height=350),chartid="m3")
  
  Vars=c("Primary Sales","Secondary Sales","Closing Stock")
  Values=c(127179,116068,40345)
  data1=data.frame(Vars,Values)
  m4=gvisGauge(data1[1,], options=list(min=0, max=180000, greenFrom=120000,greenTo=180000, yellowFrom=60000, yellowTo=120000,redFrom=0, redTo=60000,width=300, height=300),chartid="m4")
  m5=gvisGauge(data1[2,], options=list(min=0, max=180000, greenFrom=120000,greenTo=180000, yellowFrom=60000, yellowTo=120000,redFrom=0, redTo=60000,width=300, height=300),chartid="m5")
  m6=gvisGauge(data1[3,], options=list(min=0, max=180000, redFrom=120000,redTo=180000, greenFrom=60000, greenTo=120000,yellowFrom=0, yellowTo=60000,width=300, height=300),chartid="m6")
  m4m5=gvisMerge(m4,m5,horizontal=TRUE,chartid="m4m5")
  m4m5m6=gvisMerge(m4m5,m6,horizontal=TRUE,,chartid="m4m5m6")
  m1m2=gvisMerge(m1,m2,horizontal=TRUE,chartid="m1m2")
  m3m4m5m6=gvisMerge(m3,m4m5m6,horizontal=FALSE,chartid="m3m4m5m6")
  m1m2m3m4m5m6=gvisMerge(m1m2,m3m4m5m6,horizontal=FALSE,,chartid="m1m2m3m4m5m6")
  m1m2m3m4m5m6$html$chart["divChart"]="\n<table border=\"0\">\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n\n
								
										<!-- divChart -->\n  
										\n<h4>Sales Contribution by State</h4>\n
										\n<div id=\"m1\"\n  
										style=\"width: 450px; height: 300px;\">
										\n</div>\n\n</td>\n<td>\n\n
								
										<!-- divChart -->\n 
										\n<h4>Sales Growth by State</h4>\n
										\n<div id=\"m2\"\n  
										style=\"width: 450px; height: 300px;\">\n</div>\n\n</td>\n</tr>
										\n</table>\n\n</td>\n</tr>\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n\n
								
										<!-- divChart -->\n  
										\n<h4>Sales Trends over Time</h4>\n
										\n<div id=\"m3\"\n  
										style=\"width: 900px; height: 350px;\">
										\n</div>\n\n</td>\n</tr>\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n\n
								
										\n<h4>Performance Tracker</h4>\n
										<!-- divChart -->\n 	
										\n<div id=\"m4\"\n  
										style=\"width: 300px; height: 300px;\">
										\n</div>\n\n</td>\n<td>\n\n
										
										\n<h4>&nbsp</h4>\n
										<!-- divChart -->\n  
										\n<div id=\"m5\"\n  
										style=\"width: 300px; height: 300px;\">
										\n</div>\n\n</td>\n</tr>\n</table>\n\n</td>\n<td>\n\n
										
										\n<h4>&nbsp</h4>\n
										<!-- divChart -->\n  
										\n<div id=\"m6\"\n  
										style=\"width: 300px; height: 300px;\">
										\n</div>\n\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n</table>\n" 
  return(m1m2m3m4m5m6)
  })
  
output$page2 <- renderGvis({
  data1=data.frame(Region=c("KARNATAKA",as.character(mydata3$Dstributors)),Parrent=c(NA,rep("KARNATAKA",length(mydata3$Dstributors))),Sales=c(sum(mydata3$Sales),mydata3$Sales),Growth=c(10,mydata3$Growth))
  n1=gvisTreeMap(data1, "Region", "Parrent","Sales","Growth",options=list(width=450, height=450),chartid="n1")
  
  data2=data.frame(LatLon=mydata3$LatLon,Tip=paste(mydata3$Dstributors ," HAS SALES : ", mydata3$Sales))
  n2=gvisMap(data2, "LatLon" , "Tip",options=list(showTip=TRUE, showLine=TRUE ,enableScrollWheel=TRUE,mapType="terrain", useMapTypeControl=TRUE,width=450,height=450, zoomLevel=1),chartid="n2")

  n3=gvisComboChart(mydata2, xvar="Date",yvar=c("Closing_Stock","Primary_Sales","Secondary_Sales"),options=list(seriesType="bars",series='{2: {type:"line"}}',width=900,height=350),chartid="n3")
  
  Vars=c("Primary Sales","Secondary Sales","Closing Stock")
  Values=c(4542,4145,1441)
  data1=data.frame(Vars,Values)
  n4=gvisGauge(data1[1,], options=list(min=0, max=6500, greenFrom=4200,greenTo=6500, yellowFrom=2100, yellowTo=4200,redFrom=0, redTo=2100,width=300, height=300),chartid="n4")
  n5=gvisGauge(data1[2,], options=list(min=0, max=6500, greenFrom=4200,greenTo=6500, yellowFrom=2100, yellowTo=4200,redFrom=0, redTo=2100,width=300, height=300),chartid="n5")
  n6=gvisGauge(data1[3,], options=list(min=0, max=6500, redFrom=4200,redTo=6500, greenFrom=2100, greenTo=4200,yellowFrom=0, yellowTo=2100,width=300, height=300),chartid="n6")
  n4n5=gvisMerge(n4,n5,horizontal=TRUE,chartid="n4n5")
  n4n5n6=gvisMerge(n4n5,n6,horizontal=TRUE,chartid="n4n5n6")
  n1n2=gvisMerge(n1,n2,horizontal=TRUE,chartid="n1n2")
  n3n4n5n6=gvisMerge(n3,n4n5n6,horizontal=FALSE,chartid="n3n4n5n6")
  n1n2n3n4n5n6=gvisMerge(n1n2,n3n4n5n6,horizontal=FALSE,chartid="n1n2n3n4n5n6")
  n1n2n3n4n5n6$html$chart["divChart"]= "\n<table border=\"0\">\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n\n
										
										\n<h4>Distributor Performance Grid</h4>\n
										<!-- divChart -->\n
										\n<div id=\"n1\"\n
										style=\"width: 450px; height: 450px;\">
										\n</div>\n\n</td>\n<td>\n\n
										
										\n<h4>Distributor Performance Grid</h4>\n
										<!-- divChart -->\n  
										\n<div id=\"n2\"\n  
										style=\"width: 450px; height: 450px;\">
										\n</div>\n\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n\n
										
										<!-- divChart -->\n 
										\n<h4>Sales Trends over Time</h4>\n
										\n<div id=\"n3\"\n  
										style=\"width: 900px; height: 350px;\">
										\n</div>\n\n</td>\n</tr>\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n\n
										
										<!-- divChart -->\n  
										\n<h4>Performance Tracker</h4>\n
										\n<div id=\"n4\"\n  
										style=\"width: 300px; height: 300px;\">
										\n</div>\n\n</td>\n<td>\n\n
										
										<!-- divChart -->\n 
										\n<h4>&nbsp</h4>\n
										\n<div id=\"n5\"\n  
										style=\"width: 300px; height: 300px;\">
										\n</div>\n\n</td>\n</tr>\n</table>\n\n</td>\n<td>\n\n
										
										<!-- divChart -->\n 
										\n<h4>&nbsp</h4>\n
										\n<div id=\"n6\"\n  
										style=\"width: 300px; height: 300px;\">
										\n</div>\n\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n</table>\n" 
  return(n1n2n3n4n5n6)
  })

output$page3 <- renderGvis({
 
  o1=gvisMotionChart(mydata5, idvar = "Brand", timevar = "Month", yvar = "Growth",xvar = "ROI", colorvar = "Brand", sizevar = "Volume",date.format = "%Y-%m", options = list(width=700, height=350), chartid="o1")
  o2=gvisCalendar(mydata4, datevar="Date", numvar="Secondery.Sales",options=list(width=700, height=350,calendar="{ cellSize: 12 }"),chartid="o2")

  data1=data.frame(Variables=c("Sales","Stock"),Actual=c(2235,305),Target=c(3000,500))
  o3=gvisTable(data1,options=list(width=400, height=100),chartid="o3")
  
  data2=data.frame(Month=c("Mar","Apr","May","Jun"), Sales=c(1230,1587,1395,1452))
  o4=gvisLineChart(data2, xvar="Month", yvar="Sales",options=list(width=400, height=200),chartid="o4")
  
  data3=data.frame(Variable=c("Retail","Wholesale"),Value=c(1235,1157))
  o5=gvisPieChart(data3,options=list(width=400, height=300),chartid="o5")
  
  o3o4=gvisMerge(o3,o4,horizontal=FALSE,chartid="o3o4")
  o3o4o5=gvisMerge(o3o4,o5,horizontal=FALSE,chartid="o3o4o5")
  o1o2=gvisMerge(o1,o2,horizontal=FALSE,chartid="o1o2")
  o1o2o3o4o5=gvisMerge(o1o2,o3o4o5,horizontal=TRUE,chartid="o1o2o3o4o5")
  o1o2o3o4o5$html$chart["divChart"]="\n<table border=\"0\">\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n\n
									
									\n<h4>Growth V/s ROI</h4>\n
									<!-- divChart -->\n  
									\n<div id=\"o1\"\n  
									style=\"width: 700px; height: 350px;\">
									\n</div>\n\n</td>\n</tr>\n<tr>\n<td>\n\n
									
									\n<h4>Daily Sales Snapshot</h4>\n
									<!-- divChart -->\n  
									\n<div id=\"o2\"\n  
									style=\"width: 700px; height: 350px;\">
									\n</div>\n\n</td>\n</tr>\n</table>\n\n</td>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n\n
									
									\n<h4>Performance Snapshot</h4>\n
									<!-- divChart -->\n  
									\n<div id=\"o3\"\n  
									style=\"width: 400px; height: 100px;\">
									\n</div>\n\n</td>\n</tr>\n<tr>\n<td>\n\n
									
									\n<h4>Trend Of Sales</h4>\n
									<!-- divChart -->\n  
									\n<div id=\"o4\"\n  
									style=\"width: 400px; height: 200px;\">
									\n</div>\n\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n<tr>\n<td>\n\n
									
									\n<h4>Retail Vs Wholesale Sales</h4>\n
									<!-- divChart -->\n
									\n<div id=\"o5\"\n  
									style=\"width: 400px; height: 300px;\">
									\n</div>\n\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n</table>\n" 

  return(o1o2o3o4o5)
  })
  
  output$page4 <- renderGvis({
 
  
  })
  
})

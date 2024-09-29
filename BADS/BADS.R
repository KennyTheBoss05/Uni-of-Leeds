greatest = 0

for (i in 5:32)
{
  for (j in 5:32)
  {
    for (k in 5:32)
    {
      if (87*i + 81*j + 78*k <=2500 && 40*i + 28*j + 20*k <=1000 && (270*i - 180*j <=500 && 270*i - 180*j >=-500) && (270*i - 120*k <=500 && 270*i - 120*k >=-500))
      {
        val = 270*i + 180*j + 120*k
        if (val>greatest)
        {
          greatest = val
          surei = i
          surej = j 
          surek = k
        }
      }
    }
  }
}
print("Greatest value is : ")
greatest
surei
surej
surek
func = function(i,j,k)
{
  if (87*i + 81*j + 78*k <=2500 && 40*i + 28*j + 20*k <=1000)
  {
    print( 270*i + 180*j + 120*k )
  }
  else
  {
    print("Invalid")
  }
}
FUNC = function(i,j,k)
{
  print(87*i + 81*j + 78*k)
  print(40*i + 28*j + 20*k )
  print( 270*i + 180*j + 120*k )
}
FUNC(19,5,5)

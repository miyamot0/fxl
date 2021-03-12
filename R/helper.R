
getFAMarkerType = function(condition) {

  condition.upper <- toupper(condition)

  marker = switch(
    condition.upper,
    "TOY PLAY"  = 16,
    "ATTENTION" = 22,
    "DEMAND"    = 24,
    "TANGIBLE"  = 8
  )

  return (marker)
}

getFAMarkerColor = function(condition) {

  condition.upper <- toupper(condition)

  color = switch(
    condition.upper,
    "TOY PLAY"  = 'black',
    "ATTENTION" = 'white',
    "DEMAND"    = 'white',
    "TANGIBLE"  = 'black'
  )

  return (color)
}

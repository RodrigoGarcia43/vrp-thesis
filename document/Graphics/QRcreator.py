import pyqrcode 
# import png 
from pyqrcode import QRCode
import json
  
content = "@misc{\n\t" + "name: Dalianys Pérez Perera\n\t" + "title: Una propuesta para la codificación de soluciones del VRP a un espacio continuo\n\t" + 'advisors:\n\t\tMSc. Fernando Rodríguez Flores,\n\t\tLic. Alain Cartaya Salabarría\n\t' + 'year: 2021\n\t' + "university: University of Havana\n\t" + 'type: Diploma Thesis\n' + "}"

print (content)
  
#~ # Generate QR code
url = pyqrcode.create(content) 
  
# Create and save the svg file naming "myqr.svg" 
url.svg("qrcode-tesis.svg", scale = 8) 
  
# Create and save the png file naming "myqr.png" 
url.png('qrcode-tesis.png', scale = 6) 

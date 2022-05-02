type Producto = (String,Float)

--El tipado se mantiene comentado porque sino exige definir la funcion

--take :: Int -> String -> String

--drop :: Int -> String -> String

--head :: String -> Char

--elem :: Char -> String -> Bool

--reverse :: String -> String


--precioTotal: Dado un precio unitario, una cantidad, un descuento y un costo de envío calcular el precio total. Para eso, hay que calcular el precio unitario con descuento y multiplicarlo por la cantidad. ¡No te olvides de agregar el precio del envío! 
precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal (nombreProducto,precioUnitario) cantidad descuento costoEnvio = aplicarDescuento (nombreProducto,precioUnitario) descuento * cantidad + aplicarCostoDeEnvio (nombreProducto,precioUnitario) costoEnvio

--aplicarDescuento: Dado un precio y un descuento, obtener el precio final con el descuento aplicado.
aplicarDescuento :: Producto -> Float -> Float
aplicarDescuento (_,precioUnitario) descuento = precioUnitario * (1 - descuento/100)

--aplicarCostoDeEnvio: Dado un precio y un costo de envío, obtener el precio final una vez sumado el costo de envío.
aplicarCostoDeEnvio :: Producto -> Float -> Float
aplicarCostoDeEnvio (_,precioUnitario) costoEnvio = precioUnitario + costoEnvio


--entregaSencilla: Una entrega es sencilla, si se hace en un día sencillo. Los días sencillos son lo que tienen una cantidad de letras par en el nombre. Ejemplo de un día: “20 de Abril de 2020”.  
entregaSencilla :: String -> Bool
entregaSencilla fecha = even (length(fecha))


--productoDeElite: Un producto es de elite si es de lujo, codiciado y no es un producto corriente.
productoDeElite :: Producto -> Bool
productoDeElite (nombreProducto,precioUnitario) = productoDeLujo (nombreProducto,precioUnitario) && productoCodiciado (nombreProducto,precioUnitario) && not(productoCorriente (nombreProducto,precioUnitario))

--productoCodiciado: Dado el nombre de un producto, saber si es un producto codiciado. Un producto es codiciado cuando la cantidad de letras en su nombre es mayor a 10.
productoCodiciado :: Producto -> Bool
productoCodiciado (nombreProducto,_) = length nombreProducto > 10

--descodiciarProducto: Dado el nombre de un producto, generar uno que no sea codiciado. Para esto le vamos a sacar las últimas letras hasta que la cantidad de letras en el nombre quede igual a 10 (ó menor a 10 en productos con nombres cortos)
descodiciarProducto :: Producto -> String
descodiciarProducto (nombreProducto,_) = take 10 nombreProducto

--versionBarata: Dado el nombre de un producto conseguir su versión barata. La misma es el producto descodiciado y con su nombre dado vuelta.
versionBarata :: Producto -> String
versionBarata = reverse.descodiciarProducto

--productoCorriente: Dado el nombre de un producto, saber si es un producto corriente. Un producto es corriente si la primera letra de su nombre es una vocal.
productoCorriente :: Producto -> Bool
productoCorriente (nombreProducto,_) = elem (head nombreProducto) "aeiouAEIOU"

--productoXL: Dado un producto, conseguir su versión XL. Esta se consigue agregando ‘XL’ al final del nombre.
productoXL :: Producto -> String
productoXL (nombreProducto,_) = nombreProducto ++ "XL"

--productoDeLujo: Dado el nombre de un producto, saber si es de lujo. Un producto es de lujo cuando contiene una “x” o “z” en su nombre.
productoDeLujo :: Producto -> Bool
productoDeLujo (nombreProducto,_) = elem 'x' nombreProducto || elem 'z' nombreProducto

--take :: Int -> String -> String

--drop :: Int -> String -> String

--head :: String -> Char

--elem :: Char -> String -> Bool

--reverse :: String -> String
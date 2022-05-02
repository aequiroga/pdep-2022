
--precioTotal: Dado un precio unitario, una cantidad, un descuento y un costo de envío calcular el precio total. Para eso, hay que calcular el precio unitario con descuento y multiplicarlo por la cantidad. ¡No te olvides de agregar el precio del envío! 
precioTotal :: Float -> Float -> Float -> Float -> Float
precioTotal precioUnitario cantidad descuento costoEnvio = aplicarDescuento (precioUnitario * cantidad) descuento + costoEnvio


--aplicarDescuento: Dado un precio y un descuento, obtener el precio final con el descuento aplicado.
aplicarDescuento :: Float -> Float -> Float
aplicarDescuento precio descuento = precio * (1 - descuento/100)

--entregaSencilla: Una entrega es sencilla, si se hace en un día sencillo. Los días sencillos son lo que tienen una cantidad de letras par en el nombre. Ejemplo de un día: “20 de Abril de 2020”.  
entregaSencilla :: String -> Bool
entregaSencilla fecha = even (length(fecha))

--descodiciarProducto: Dado el nombre de un producto, generar uno que no sea codiciado. Para esto le vamos a sacar las últimas letras hasta que la cantidad de letras en el nombre quede igual a 10 (ó menor a 10 en productos con nombres cortos)
descodiciarProducto :: String -> String
descodiciarProducto nombreProducto = take 10 nombreProducto

--aplicarCostoDeEnvio: Dado un precio y un costo de envío, obtener el precio final una vez sumado el costo de envío.
aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio precio costoEnvio = precio + costoEnvio

--productoCodiciado: Dado el nombre de un producto, saber si es un producto codiciado. Un producto es codiciado cuando la cantidad de letras en su nombre es mayor a 10.
productoCodiciado :: String -> Bool
productoCodiciado nombreProducto = length nombreProducto > 10

--productoCorriente: Dado el nombre de un producto, saber si es un producto corriente. Un producto es corriente si la primera letra de su nombre es una vocal.
productoCorriente :: String -> Bool
--productoCorriente nombreProducto = head nombreProducto == 'a' || head nombreProducto == 'e' || head nombreProducto == 'i'  || head nombreProducto == 'o'  || head nombreProducto == 'u' 
--productoCorriente nombreProducto = elem (head nombreProducto) ['a','e','i','o','u']
productoCorriente nombreProducto = elem (head nombreProducto) "aeiouAEIOU"

--productoXL: Dado un producto, conseguir su versión XL. Esta se consigue agregando ‘XL’ al final del nombre.
productoXL :: String -> String
productoXL nombreProducto = nombreProducto ++ "XL"

--productoDeLujo: Dado el nombre de un producto, saber si es de lujo. Un producto es de lujo cuando contiene una “x” o “z” en su nombre.
productoDeLujo :: String -> Bool
productoDeLujo nombreProducto = elem 'x' nombreProducto || elem 'z' nombreProducto

--versionBarata: Dado el nombre de un producto conseguir su versión barata. La misma es el producto descodiciado y con su nombre dado vuelta.
versionBarata :: String -> String
versionBarata = reverse.descodiciarProducto

--productoDeElite: Un producto es de elite si es de lujo, codiciado y no es un producto corriente.
productoDeElite :: String -> Bool
productoDeElite nombreProducto = productoDeLujo nombreProducto && productoCodiciado nombreProducto && not(productoCorriente nombreProducto)

--take :: Int -> String -> String

--drop :: Int -> String -> String

--head :: String -> Char

--elem :: Char -> String -> Bool

--reverse :: String -> String

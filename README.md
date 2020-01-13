# Descripción

Caja de herramientas para el procesamiento de la [Encuesta Continua de Hogares de Uruguay](http://www.ine.gub.uy/encuesta-continua-de-hogares1) realizada por en Instituto Nacional de Estadística (INE).

# Motivación

Este paquete pretende contribuir a la comunidad de usuaries de R en Uruguay, facilitando el uso de una de las encuestas socioeconómicas más importantes del país. En este sentido, el paquete implementa una serie de funciones que permiten el cálculo de los principales indicadores socioeconómicos que permite la encuesta.


# Modo de uso

El paquete ech permite descargar los microdatos oficiales desde la web del INE o bien utilizar microdatos ya procesados.  En el primer caso los datos se obtiene a través de la función get_microdata(). En el segundo caso, si la persona ha modificado los nombres originales de las variables deberá indicarlo en cada función que utilice.

El uso de este paquete se potencia al usarlo en conjunto con [geouy](https://github.com/RichDeto/geouy) posibilitando mapear utilizando diferentes capas geográficas de Uruguay.

# Instalación


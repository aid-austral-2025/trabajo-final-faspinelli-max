# Trabajo Final

## Descripción

### Selección de datos

Se ha seleccionado para realizar el trabajo final de la materia **Análisis Inteligente de Datos** un dataset real de un club que contiene información sobre las solicitudes de compras de los distintos sectores y disciplinas.

Oiginalmente, la información esta en un servidor web en postgre. Usaremos una vista de la misma en formato excel que contendrá la cabecera de las solicitudes.

No se tendrán en cuenta para este trabajo, el detalle de esas solicitudes (los conceptos solicitados) ni los sectores por los que fue pasando la misma.

\
Las solicitudes deben ir pasando entre distintos sectores y asignándose posibles estados preconfigurados.

Existe un sector (Gerencia) que se ocupa de aprobar, modificar o rechazar la solicitud luego de que la misma haya sido cotizada por el sector compras . Si la misma es rechazada finaliza con ese estado, sino continua el circuito hacia los siguientes sectores.

Existen dos circuitos bien diferenciados para las solicitudes según que sector que la genera. Si el sector que genera la solicitud pertenece a las disciplinas del club, el encargado de cotizar es **coordinacion deportes.** Si el sector es interno al club (rrhh, pañol, etc.) el encargado de cotizar es el sector **compras**.

### 

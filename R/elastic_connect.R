library(elastic)
connection <- elastic::connect()
elastic::docs_bulk(conn = connection, x = dataset, index = "gspc8")

elastic::docs_bulk_delete(conn = connection, x = dataset, index = "gspc8", id = "GSPC")

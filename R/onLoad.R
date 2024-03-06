.onLoad <- function(libname, pkgname) {
	# These generics are exported only as of R-4.4
	registerS3method('recvData', 'nodepool_node', recvData.nodepool_node, loadNamespace('parallel'))
	registerS3method('sendData', 'nodepool_node', sendData.nodepool_node, loadNamespace('parallel'))
	registerS3method('recvOneData', 'nodepool_cluster', recvOneData.nodepool_cluster, loadNamespace('parallel'))
}

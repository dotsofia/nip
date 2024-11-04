file(GLOB nip_src CONFIGURE_DEPENDS "*.cpp")

add_executable(nip ${nip_src})

llvm_map_components_to_libnames(llvm_libs core)

target_link_libraries(nip ${llvm_libs})

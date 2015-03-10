i=320$1
/usr/bin/docker run -t                             \
          --volumes-from vmxmcr                              \
          -v /home/core/vmx/data:/vmx/data                   \
          --volumes-from vmxserver                           \
          --name vmxservernode-$1                            \
          --rm                                               \
          visionai/vmx-server-node                           \
          /vmx/build/VMXserver /dockerscratch/data sid.%i none :320$1


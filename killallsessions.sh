rm -rf /www/vmx/sessions/*
for z in `ps aux | grep VMX | grep -v grep | awk {'print $2'}`; do kill $z; done

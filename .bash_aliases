alias python=python3
alias odmount='sshfs dcervone@odyssey.fas.harvard.edu:/n/home09/dcervone/ ~/mounts/odyssey'
alias odunmount='fusermount -u ~/mounts/odyssey'
alias bornnunmount='fusermount -u ~/mounts/bornn'
alias odlogin='ssh -Y dcervone@odyssey.fas.harvard.edu'
alias bornnlogin='ssh -Y dcervone@odyssey.fas.harvard.edu:/n/gstore/Labs/Bornn_Lab'
alias bornnmount='sshfs dcervone@odyssey.fas.harvard.edu:/n/gstore/Labs/Bornn_Lab ~/mounts/bornn'
alias auth='bash ~/mounts/dcervone-openauth/dcervone-openauth.sh &' 
alias daneinor='rsync -avr --rsh="ssh -p2222" /home/dan/Dropbox/wedding/dan einor@192.254.250.162:public_html'

codesync() {
  # sync U drive *.R, *.py, *.sh code only
  aws s3 sync ~/U $MYAWS --exclude "*" --include "*.R" --include "*.py" --include "*.sh" "$@"
  aws s3 sync $MYAWS $PUBAWS --exclude "*" --include "*.R" --include "*.py" --include "*.sh" "$@"
 }

alias Qmount='sudo mount -t cifs -o username=DanielC,uid=1001,gid=1001 //dodgercifs/public ~/Q'
alias Umount='sudo mount -t cifs -o username=DanielC,uid=1001,gid=1001 //dodgercifs/Users_data/DanielC ~/U'
alias Qunmount='sudo umount ~/Q'
alias Uunmount='sudo umount ~/U'
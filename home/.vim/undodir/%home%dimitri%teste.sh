Vim�UnDo� p	(?�����1OGwJ4a��Wn��W�W$��      whoami                              d*�R    _�                             ����                                                                                                                                                                                                                                                                                                                                                             d*^V     �                 echo�                   5��                        !                   !       �                                              �        +                  +                      �                          ,                      �                         ,                     �                         ,                     �                         ,                     �                         ,                     5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       d*^�     �                 echo "message"5��                        2                     5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       d*^�     �                 echo�                 echo "$disk"5��                         8                      �                          9                      �                         H                      �                          I                      �                         U                      �                          V                      �                       &   W               &       �       &                  }                      �                          ~                      �                         �                      �                          �                      �                         �                      �                          �                      �                         �                      �                         �                      �                         �                     �                         �                     �                         �                     �                         �                     5�_�                    	       ����                                                                                                                                                                                                                                                                                                                            	          	          v       d*_     �                 echo "message"5��                        �                     5�_�                    	       ����                                                                                                                                                                                                                                                                                                                            	          	          v       d*_     �                 echo "options root"5��                        �                     5�_�                    	       ����                                                                                                                                                                                                                                                                                                                            	          	          v       d*_     �                 echo "options root'5��                        �                     5�_�                    	       ����                                                                                                                                                                                                                                                                                                                            	          	          v       d*_     �                 echo 'options root'5��                         �                      5�_�      	              	       ����                                                                                                                                                                                                                                                                                                                            	          	          v       d*_     �                 echo 'options root'"5��                         �                      5�_�      
           	   	   &    ����                                                                                                                                                                                                                                                                                                                            	          	          v       d*_/    �   
              cat ./output�                 &echo 'options root=PARTUUID=$part rw'"5��       &                  �                      �    	                      �                      �    
                      �                      �    
                     �                      �    
   
                  �                      �    
   	                  �                      �    
                     �                      �    
                     �                      �    
                    �                     �    
                    �                     �    
                    �                     �    
                     �                      �                          �                      5�_�   	              
           ����                                                                                                                                                                                                                                                                                                                                                             d*_|     �                part="$disk""2"5��                          9                      5�_�   
                         ����                                                                                                                                                                                                                                                                                                                                                             d*_     �                echo "$part"5��                          9                      5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             d*_�     �               partuuid=$(ls -l /dev�         
      echo "$disk"5��                         8                      �                          9                      �                        E                     �                        K                     �                        K                     �                        O                     �                        O                     �                        T                     5�_�                       '    ����                                                                                                                                                                                                                                                                                                                                                             d*_�     �               (partuuid=$(ls -l /dev/disk/by-partuuid/)5��       '                  `                      �       /                 h                     5�_�                       5    ����                                                                                                                                                                                                                                                                                                                                                             d*_�     �               echo�               5partuuid=$(ls -l /dev/disk/by-partuuid/ | grep $disk)5��       5                  n                      �                          o                      �                         o                     �                         o                     �                         o                     �                         o                     5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       d*_�     �               echo "message"5��                        u                     �                         w                      �                        v                     �                        v                     �                        v                     5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       d*_�     �               echo�               echo "$partuuid"5��                                               �                          �                      �                         �                      �                         �                      �                         �                      �                         �                     �                         �                     �                         �                     �                         �                     5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       d*_�     �               echo "message"5��                        �                     5�_�                           ����                                                                                                                                                                                                                                                                                                                                                v       d*_�     �                echo " "5��                          �       	               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                v       d*_�     �             �             5��                          �               	       5�_�                            ����                                                                                                                                                                                                                                                                                                                                                v       d*_�     �             �             5��                          �               	       5�_�                       4    ����                                                                                                                                                                                                                                                                                                                                                v       d*_�    �               5partuuid=$(ls -l /dev/disk/by-partuuid/ | grep $disk)5��       4                  m                      5�_�                       @    ����                                                                                                                                                                                                                                                                                                                                                             d*`    �               Epartuuid=$(ls -l /dev/disk/by-partuuid/ | grep $disk | cut -d' '-f10)5��       @                  y                      5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             d*��     �                 +disk=$(df . | grep /dev/sd | cut -d' ' -f1)   echo "$disk"   Fpartuuid=$(ls -l /dev/disk/by-partuuid/ | grep $disk | cut -d' ' -f10)   echo "$partuuid"       echo " "   echo " "   &cat > ./output <<< "title   Arch Linux   linux   /vmlinuz-linux   initrd  /initramfs-linux.img   &echo 'options root=PARTUUID=$part rw'"       cat ./output   rm ./output5��                                  @             5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             d*��    �                   5��                                                  �                                                �                                                �                                                �                                                �                                                �                       
                  
       �       
                                        �                                                5�_�                             ����                                                                                                                                                                                                                                                                                                                                                             d*�Q    �                 
su dimitri   whoami�                 whoami5��       
                                        �       
                                        5��
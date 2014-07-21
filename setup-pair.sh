#!/usr/bin/env bash
# Original Author: Luca Belmondo
# Original source: https://github.com/lucabelmondo/dotfiles/blob/master/scripts/tmux/2-tmux-user-and-ssh-setup

set -e

PAIR_USER=pair
SSHD_CONFIG=/etc/ssh/sshd_config

echo
echo "--- Installing SSH ---"
echo

sudo apt-get install -y ssh

echo
echo "--- Setting up pairing user ---"
echo

if [[ $(awk -F":" '{ print $1 }' /etc/passwd | grep $PAIR_USER) ]]; then
  sudo userdel $PAIR_USER -r
  echo "Removed existing user $PAIR_USER"
fi

sudo useradd $PAIR_USER -m -s /bin/bash
echo "Created user $PAIR_USER"

echo
echo "--- Configuring SSH server (${SSHD_CONFIG}) ---"
echo

update_or_append () {
  local OPTION_FILE=$1
  local OPTION_NAME=$2
  local OPTION_VALUE=$3

  if [[ $(grep $OPTION_NAME $OPTION_FILE) ]]; then
    sudo sed -i.bkp -E --follow-symlinks \
      "s|^\s*#*\s*${OPTION_NAME}.*|${OPTION_NAME} ${OPTION_VALUE}|g" \
      $OPTION_FILE
    echo "In ${OPTION_FILE}: Ensure existing option '${OPTION_NAME}' set to '${OPTION_VALUE}'."
  else
    echo >> $OPTION_FILE
    sudo su -c "echo '${OPTION_NAME} ${OPTION_VALUE}' >> ${OPTION_FILE}"
    echo "In ${OPTION_FILE}: Missing option ${OPTION_NAME} added with value '${OPTION_VALUE}'."
  fi
}

update_or_append $SSHD_CONFIG PermitRootLogin         no
update_or_append $SSHD_CONFIG RSAAuthentication       yes
update_or_append $SSHD_CONFIG PubkeyAuthentication    yes
update_or_append $SSHD_CONFIG AuthorizedKeysFile      "%h/.ssh/authorized_keys"
update_or_append $SSHD_CONFIG PasswordAuthentication  no
update_or_append $SSHD_CONFIG UsePAM                  yes

sudo service ssh restart

echo
echo "--- Setting up .ssh in pair user home ---"
echo

sudo mkdir -v -p "/home/${PAIR_USER}/.ssh"
echo "Ensure .ssh dir"
sudo touch "/home/${PAIR_USER}/.ssh/authorized_keys"
echo "Ensure .ssh/authorized_keys file"
sudo chmod 0600 "/home/${PAIR_USER}/.ssh/authorized_keys"
echo "Ensure correct permissions for .ssh/authorized_keys file"
sudo chown -R "${PAIR_USER}:${PAIR_USER}" "/home/${PAIR_USER}/.ssh"
echo "Ensure correct user for .ssh/authorized_keys file"

echo
echo "--- What to do next ---"
echo
echo "1 - Find your IP with ifconfig and give it to the pair"
echo "2 - Ask the pair to give you her public SSH key (with cat ~/.ssh/id_rsa.pub)"
echo "3 - Add her public key to the authorized_keys of the \"${PAIR_USER}\" user with:"
echo "    (notice quotes and double quotes)"
echo
echo "    sudo bash -c 'echo \"<....her-public-key....>\" >> /home/${PAIR_USER}/.ssh/authorized_keys'"
echo
echo "4 - Set your machine up with either tmux or custom tmux scripts (see step 3 of these scripts)"
echo "5 - ask the pair to connect with ssh ${PAIR_USER}@<your-ip-address>"

#!/usr/bin/env bash

esy dune build fan_control/fan_control.exe

for IP in $OCXMR_HOSTS; do
  ping -c1 -w1 $IP >/dev/null
  if [ $? -eq 0 ]; then
    # Create /opt/ocxmr directory
    ssh root@$IP "mkdir -p /opt/ocxmr/bin"

    # Copy r710_fc executable
    scp $(esy echo '#{self.target_dir}/default/fan_control/fan_control.exe') root@$IP:/opt/ocxmr/bin/r710_fc
    # Copy fan-control.service
    scp fan_control/fan-control.service root@$IP:/etc/systemd/system/fan-control.service
    # Enable & start fan-control.service
    ssh root@$IP "systemctl enable /etc/systemd/system/fan-control.service; systemctl start fan-control.service"
  else
    echo "Failed to ping $IP"
  fi
done

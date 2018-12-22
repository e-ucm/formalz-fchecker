## Deployment Instructions

1. Clone the `javawlp` repository in `/root/javawlp/`.

2. Install `immortal` ([instructions](https://immortal.run/post/how-to-install/))

3. Setup `immortaldir` using `systemd` ([instructions](https://immortal.run/post/systemd/)), storing the following service configuration in `/etc/systemd/system/immortaldir.service`:

```yaml
[Unit]
Description=immortaldir
After=network.target

[Service]
ExecStart=/usr/bin/immortaldir /root/javawlp/immortal 2>&1 | logger -t immortaldir
KillMode=process
Restart=always
Restart=on-failure
Type=simple
User=root

[Install]
WantedBy=multi-user.target
```

4. Everytime you need to deploy, run `deploy.sh` which will also pull the last commit.

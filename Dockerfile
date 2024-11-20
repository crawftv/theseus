# Use Debian as the base image
FROM 637423200832.dkr.ecr.us-east-1.amazonaws.com/crawfordc:crawford-base AS profile_base

WORKDIR /app
RUN pip install fastapi uvicorn libsql-experimental

COPY server.py /app/server.py
COPY /static /app/static
COPY start_tunnel.sh /app/start_tunnel.sh
RUN chmod +x /app/start_tunnel.sh
COPY supervisor.conf 	/etc/supervisor/conf.d/supervisord.conf
# Add command to start Nginx
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]

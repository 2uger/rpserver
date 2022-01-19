FROM python:3

# set working directory
WORKDIR /rpserver
# copy all files from current dir to container
COPY . .
RUN pip install --no-cache-dir -r requirements.txt

EXPOSE 5000
CMD ["python", "run.py"]

# workouttracker
 
RShiny App to track gym workout progress (still work inprogress).


## Docker

Build docker image with: `docker build -t workouttracker-image .`

Run docker container with `docker run -d --name workouttracker -p 3838:3838 workouttracker-image`.

## AWS EC2

Copy dropbox oauth key
`scp -i workouttracker.pem ~/programming/workouttracker/workouttracker/droptoken.rds ubuntu@ec2-35-158-161-220.eu-central-1.compute.amazonaws.com:/home/ubuntu/workouttracker`

`sudo service docker start`
`sudo docker info`
`sudo docker build -t workouttracker-image .`
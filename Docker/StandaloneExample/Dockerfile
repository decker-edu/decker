#Ubuntu just provides the the best toolbox for decker, the offical Haskell immage doesn't have all dependencies
#The 17.10 as a rolling release is used because of the sass-compiler
# source of the missing dependecy: https://askubuntu.com/questions/849057/how-to-install-libsass-on-ubuntu-16-04
# if the 18:04 LTS is available this one is automatically used, maybe then it can be set it as a fixed base-mage
FROM ubuntu:rolling

#update the packages
RUN apt-get update

#install all haskell-stack dependencies, for some reasons an extra gcc is required
RUN	apt-get install -y xz-utils && \
	apt-get install -y gcc && \
	apt-get install -y wget && \
	apt-get install -y git
#install haskell-stack
RUN	wget -qO- https://get.haskellstack.org/ | sh
#install all decker dependencies
RUN	apt-get install -y unzip zip && \
	apt-get install -y rsync && \
	apt-get install -y graphviz && \
	apt-get install -y gnuplot && \
	apt-get install -y sassc

#for pdf-features:
RUN	apt-get install -y pdf2svg && \
	apt-get install -y texlive-latex-base




#cloning the decker repo with HTTPS to avoid SSH-Key hazzle, otherwise the SSH-key has to be added to the container with 'ADD'
RUN git clone https://gitlab2.informatik.uni-wuerzburg.de/decker/decker.git
#change to the decker source-folder
WORKDIR /decker
#the git checkout is a stable release under ubuntu, maybe this could be solved nicer
#a checkout by a tag would of course be better
RUN git checkout cb68693de73576cff738a531a65229191351030b

#install decker from the source
RUN make install

#add the location of decker binary to the PATH
ENV PATH="/root/.local/bin:${PATH}"

#set the encoding on UTF-8, so the parser works correctly, german language is also added for umlaute
#source of fix: https://blog.mkowalski.net/2016/05/16/solving-locale-issues-with-docker-containers/
RUN apt-get install --reinstall -y locales && \
	sed -i 's/# de_DE.UTF-8 UTF-8/de_DE.UTF-8 UTF-8/' /etc/locale.gen && \
	locale-gen de_DE.UTF-8
ENV LANG de_DE.UTF-8
ENV LANGUAGE de_DE
ENV LC_ALL de_DE.UTF-8
RUN dpkg-reconfigure --frontend noninteractive locales

#cleanup to get a smaller image
RUN apt-get clean autoclean && \
	 apt-get autoremove -y

#an example decker build, starting at container-runtime
CMD [ "sh", "-c", "cd /; decker example; cd example/; decker server" ]

#open the port for mapping
Expose 8888

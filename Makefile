default: clean release

release:
	lazbuild --build-mode=Release -B *.lpi

debug:
	lazbuild --build-mode=Debug -B *.lpi

clean:
	rm -Rf lib
	rm -Rf bin

zip:
	rm sources.zip
	zip -r sources.zip * -x "bin*" "lib*" ".git*" "*backup*" ".github*"
	

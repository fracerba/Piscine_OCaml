SOURCES = people.ml doctor.ml main.ml
RESULT  = ex01
OCAMLOPT = ocamlopt

all: $(RESULT)

$(RESULT): $(SOURCES)
	$(OCAMLOPT) -o $@ $^

clean:
	rm -f *.cm* *.o

fclean: clean
	rm -f $(RESULT)

re: fclean all

.PHONY: all clean fclean re
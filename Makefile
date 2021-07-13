##
## EPITECH PROJECT, 2021
## B-FUN-400-PAR-4-1-wolfram-charlie.chou
## File description:
## Makefile
##

NAME		=		ImgComp-exe

REAL_NAME	=		imageCompressor

STACK		=		stack

CP			=		cp

all:
	$(STACK) build
	$(CP) `$(STACK) path --local-install-root`/bin/$(NAME) ./
	mv $(NAME) $(REAL_NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(REAL_NAME)

re: fclean all

.PHONY:	all fclean clean re
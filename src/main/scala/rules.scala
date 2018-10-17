
package ll {
	object util {
		def next[T](els: Seq[T]): Seq[T] =
			if(els.length <= 1) els
			else {
				val h = els.take(1)
				els.drop(1) ++ h
			}
	}

	package game {
		/* entities */
		
		sealed trait Entity

		case object Bottom extends Entity

		trait Choice {
			def apply(context: Seq[Entity]): Entity
		}

		trait Card extends Entity {
			val value: Int
		}

		case class ActualCard(val c: Card) extends Entity {
			val id: Int = scala.util.Random.nextInt
			def value = c.value
		}

		case class Player(val name: String, val hand: Seq[ActualCard], 
			val discard: Seq[ActualCard], val protection: Boolean = false) extends Entity {
			
			override def equals(o: Any) = o match {
				case that: Player => that.name.equalsIgnoreCase(this.name)
				case _ => false
			}

			override def hashCode = name.toUpperCase.hashCode

			def has(c: Card) = hand.contains(ActualCard(c))
			def hasExact(a: ActualCard) = hand.map { _.id == a.id }

			def find(c: Card) = hand.filter { _ == ActualCard(c) }.headOption

			def protect = this.copy(protection = true)
			def unprotect = this.copy(protection = false)
			
			def throwOut(a: ActualCard): Player = throwOut(a.c)

			def throwOut(c: Card): Player = {
				find(c) match {
					case Some(instance) =>
						val newHand = hand.filter { _.id != instance.id }
						val newDiscard = Seq(instance) ++ discard

						this.copy(hand = newHand, discard = newDiscard)
					case None => this
				}
			}
		}

		case class Over(p: Option[Entity])
		object Over {
			def apply(p: Entity): Over = Over(Option(p))
		}
		
		package object cards {
			case object Guard extends Card { val value = 1 }
			case object Priest extends Card { val value = 2 }
			case object Baron extends Card { val value = 3 }
			case object Handmaid extends Card  { val value = 4 }
			case object Prince extends Card  { val value = 5 }
			case object King extends Card { val value = 6 }
			case object Countess extends Card { val value = 7 } 
			case object Princess extends Card { val value = 8 }

			object Cards {
				def all = Seq(Guard, Priest, Baron, Handmaid,
					Prince, King, Countess, Princess)

				def nonGuard = Seq(Priest, Baron, Handmaid,
					Prince, King, Countess, Princess)
			}
		}

		/* continuations (to remember that something else 
		  		  needs to happen after an action) */

		sealed trait Kont {
			def ::(k: Kont) = Seq(this, k).toList
		}

		case class DrawK(subject: Player) extends Kont
		case class ChooseCardK(subject: Player, choice: Seq[Card]) extends Kont
		case class ChooseAK(subject: Player, choice: Seq[ActualCard]) extends Kont
		case class ChoosePlayerK(subject: Player, choice: Seq[Player]) extends Kont
		case class CompareK(best: Player, card: Int) extends Kont
		case object StartingRoundK extends Kont
		case object PlayingCardK extends Kont 

		case class GuardSelectingCardK() extends Kont
		case class GuardSelectingPlayerK(card: Card) extends Kont
		case class PriestSelectingPlayerK() extends Kont
		case class BaronSelectingPlayerK() extends Kont
		case class PrinceSelectingPlayerK() extends Kont
		case class KingSelectingPlayerK(c: Card) extends Kont

		/* states */

		case class State(
			current: Entity, 
			players: Seq[Player], 
			deck: Seq[ActualCard], 
			kont: List[Kont]) {

			def discard(c: ActualCard, who: Option[Player] = players.headOption) = {
				val changed = who match {
					case Some(player) => 
						players.map((pl: Player) => if(player == pl) pl.throwOut(c) else player)
					case None =>
						players
				}

				this.copy(players = changed)
			}

			def next = this.copy(players = ll.util.next(players))
		}

		/* interpreter */

		class Interpreter(choiceFns: Map[Player, Choice]) {

			def starting(players: Seq[Player], deck: Seq[ActualCard]): State = 
				State(Bottom, players, deck, List[Kont]())

			def checkPrincessDiscarded(players: Seq[Player]) =
				players.map { _.discard.contains(ActualCard(cards.Princess)) }.contains(true)

			def peopleWhoDidntDiscardPrincess(players: Seq[Player]): Seq[Player] =
				players.filter { !_.discard.contains(ActualCard(cards.Princess)) }

			def checkEmptyHands(players: Seq[Player]) =
				players.map { _.hand.isEmpty }.contains(true)

			def peopleWithoutEmptyHands(players: Seq[Player]): Seq[Player] =
				players.filter { !_.hand.isEmpty }

			def next(s: State): Either[State, Over] = s match {
				// no players left: no one wins
				case State(_, Nil, _, _) =>
					Right(Over(Bottom))

				// one player left: they win automatically
				case State(_, p :: rest, _, _) if rest.isEmpty =>
					Right(Over(p))

				// no more cards to draw: compare hands; best wins
				case State(_, p :: rest, Seq(), k) =>
					if(p.hand.isEmpty)
						Left(s.copy(players = rest))
					else
						Left(s.copy(players = rest, kont = CompareK(p, p.hand.head.value) :: k))

				// out due to empty hand
				case State(_, players, _, _) if checkEmptyHands(players) =>
					Left(s.copy(players = peopleWithoutEmptyHands(players)))

				// discarding princess is the end
				case State(_, players, _, _) if checkPrincessDiscarded(players) =>
					Left(s.copy(players = peopleWhoDidntDiscardPrincess(players)))

				// start of the round
				case State(Bottom, p :: rest, _, Nil) =>
					Left(s.copy(players = p.unprotect :: rest, kont = DrawK(p) :: StartingRoundK))

				case State(Bottom, p :: rest, _, StartingRoundK :: k) =>
					Left(s.copy(kont = ChooseAK(p, p.hand) :: PlayingCardK :: k))

				case State(a: ActualCard, p :: rest, _, PlayingCardK :: k) =>
					val afterPlaying = p.throwOut(a)
					Left(s.copy(kont = k))

				// someone draws a card
				case State(Bottom, players, _, DrawK(p) :: k) =>
					val top = s.deck.head
					val deck = s.deck.drop(1)
					val pc = p.copy(hand = p.hand :+ top)
					val cts = if(pc.has(cards.Countess) && 
						(pc.has(cards.King) || pc.has(cards.Prince)))
						pc.throwOut(cards.Countess)
					else pc
					val pls = players.map { case q if p.name == q.name => cts; case q => q }
					Left(State(Bottom, pls, deck, k))

				// someone chooses a player
				case State(_, players, deck, ChoosePlayerK(p, choice) :: k) =>
					val pick = choiceFns(p)
					val chosen = pick(choice)
					Left(State(chosen, players, deck, k))

				// someone chooses a card
				case State(_, players, deck, ChooseCardK(p, choice) :: k) =>
					val pick = choiceFns(p)
					val chosen = pick(choice)
					Left(State(chosen, players, deck, k))

				// someone chooses an actual card
				case State(_, players, deck, ChooseAK(p, choice) :: k) =>
					val pick = choiceFns(p)
					val chosen = pick(choice)
					Left(State(chosen, players, deck, k))

				// the chosen card is there, but why?
				case State(ActualCard(c), p :: rest, deck, k) =>
					import cards._

					val players = p :: rest

					val ns = (c, k) match {

						// guard

						case (Guard, Nil) =>
							s.copy(kont = ChooseCardK(p, Cards.nonGuard) :: GuardSelectingCardK())

						case (cc:Card, GuardSelectingCardK() :: k) =>
							s.copy(kont = ChoosePlayerK(p, rest) :: GuardSelectingPlayerK(cc) :: k)

						case (cp:Player, GuardSelectingPlayerK(chosenCard) :: k2) if !cp.protection =>
							val remaining = if(cp.has(chosenCard)) cp.throwOut(chosenCard) else cp
							val pls = players.map(x => if(cp == x) remaining else x)

							State(Bottom, pls, deck, k2).next

						// priest

						case (Priest, Nil) =>
							s.copy(kont = ChoosePlayerK(p, rest) :: PriestSelectingPlayerK())

						case (cp:Player, PriestSelectingPlayerK() :: k2) if !cp.protection =>
							println("You see that the player you've chosen has the following hand:")
							cp.hand.map(crd => println(s"${crd}"))

							State(Bottom, players, deck, k2).next

						// baron
						
						case (Baron, Nil) =>
							s.copy(kont = ChoosePlayerK(p, rest) :: BaronSelectingPlayerK())

						case (cp:Player, BaronSelectingPlayerK() :: k2) if !cp.protection =>
							val nextOrder = ll.util.next(players)
							val remaining = (cp.hand.head.value - p.hand.head.value) match {
								case x if x < 0 => nextOrder.filter(_ != p)
								case x if x > 0 => nextOrder.filter(_ != cp)
								case x if x == 0 => nextOrder
							}

							State(Bottom, remaining, deck, k2)

						// handmaid

						case (Handmaid, Nil) =>
							State(Bottom, p.copy(protection = true) :: rest, deck, Nil).next

						// prince

						case (Prince, Nil) =>
							s.copy(kont = ChoosePlayerK(p, players) :: PrinceSelectingPlayerK())

						case (cp:Player, PrinceSelectingPlayerK() :: k2) if !cp.protection =>
							val affected = cp.copy(hand = Seq(), 
								discard = cp.hand ++ cp.discard)
							val pls = players.map(x => if(cp == x) affected else x)

							State(Bottom, pls, deck, DrawK(affected) :: k2).next

						// king

						case (King, Nil) =>
							s.copy(kont = ChoosePlayerK(p, players) :: KingSelectingPlayerK(c))

						case (cp:Player, KingSelectingPlayerK(crd) :: k2) if !cp.protection =>
							val cpo = cp.copy(hand = p.hand)
							val po = p.copy(hand = cp.hand)
							val pls = players.map(x => x match {
								case y if y == cp => cpo
								case y if y == p => po
								case y => y 
							})

							State(Bottom, pls, deck, k2).next

						// countess 

						case (Countess, Nil) =>
							s.next

						// princess: we play her, we're out

						case (Princess, Nil) =>
							State(Bottom, rest, deck, Nil)

						// stuck case is stuck

						case _ => s

					}

					Left(ns)
			}
		}
	}
}

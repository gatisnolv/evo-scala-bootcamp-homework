package com.gatis.homework.error_handling

import java.time._
import cats.data.ValidatedNec
import cats.syntax.all._
import scala.util.Try

object ErrorHandling extends App {

  object Homework {

    case class PaymentCard(name: String, number: String, expirationDate: YearMonth, securityCode: String)
    object PaymentCard {
      import PaymentCardValidator._
      def of(name: String, number: String, expirationDate: String, securityCode: String): AllErrorsOr[PaymentCard] = validate(name, number, expirationDate, securityCode)
    }

    sealed trait ValidationError
    object ValidationError {
      final case object NameOnCardHasUnsupportedCharacters extends ValidationError {
        override def toString = "The name on credit card contains unsupported characters"
      }
      final case object NameOnCardHasIncorrectLength extends ValidationError {
        override def toString = "The name on card has to be between 2 and 26 characters long"
      }
      final case object CardNumberIsNotNumeric extends ValidationError {
        override def toString = "Credit card number must contain only digits"
      }
      final case object CardNumberHasWrongLength extends ValidationError {
        override def toString = "Credit card number length must be between 15 and 19 digits long"
      }
      final case object ExpirationDateHasInvalidFormat extends ValidationError {
        override def toString = "Expiration dates must be valid dates in the form MM/YY"
      }
      final case object CardHasExpired extends ValidationError {
        override def toString = "Credit cards must not be expired"
      }
      final case object SecurityCodeIsNotNumeric extends ValidationError {
        override def toString = "The security code must be numeric"
      }
      final case object SecurityCodeHasWrongLength extends ValidationError {
        override def toString = "The security code must be 3 or 4 digits long"
      }
    }

    object PaymentCardValidator {

      import ValidationError._

      type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

      private def validateName(name: String): AllErrorsOr[String] = {
        def validateNameContents: AllErrorsOr[String] =
          if (name.matches("^[A-Z '`~.-]+$")) name.validNec
          else NameOnCardHasUnsupportedCharacters.invalidNec

        def validateNameLength: AllErrorsOr[String] =
          if (name.length > 26 || name.length < 2) NameOnCardHasIncorrectLength.invalidNec
          else name.validNec

        validateNameContents *> validateNameLength
      }

      private def validateNumber(number: String): AllErrorsOr[String] = {
        def validateNumberContents: AllErrorsOr[String] =
          if (number.matches("^[0-9]+$")) number.validNec
          else CardNumberIsNotNumeric.invalidNec

        def validateNumberLength: AllErrorsOr[String] =
          if (number.length > 19 || number.length < 15) CardNumberHasWrongLength.invalidNec
          else number.validNec

        validateNumberContents *> validateNumberLength
      }

      private def validateExpirationDate(expirationDate: String): AllErrorsOr[YearMonth] = {
        def validateDateFormat: AllErrorsOr[YearMonth] = {
          expirationDate.split("/") match {
            case Array(month, year) =>
              Try(YearMonth.of(2000 + year.toInt, month.toInt)).toOption match {
                case Some(x) => x.validNec
                case None    => ExpirationDateHasInvalidFormat.invalidNec
              }
            case _ => ExpirationDateHasInvalidFormat.invalidNec
          }
        }

        def validateCardNotExpired(date: YearMonth) =
          if (date.isAfter(YearMonth.now()))
            date.validNec
          else CardHasExpired.invalidNec

        validateDateFormat andThen validateCardNotExpired
      }

      private def validateSecurityCode(code: String): AllErrorsOr[String] = {
        def validateCodeContents: AllErrorsOr[String] =
          if (code.matches("^[0-9]+$")) code.validNec
          else SecurityCodeIsNotNumeric.invalidNec

        def validateCodeLength: AllErrorsOr[String] =
          if (code.length > 4 || code.length < 3) SecurityCodeHasWrongLength.invalidNec
          else code.validNec

        validateCodeContents *> validateCodeLength
      }

      def validate(name: String, number: String, expirationDate: String, securityCode: String): AllErrorsOr[PaymentCard] =
        (validateName(name), validateNumber(number), validateExpirationDate(expirationDate), validateSecurityCode(securityCode)).mapN(PaymentCard(_, _, _, _))
    }
  }
}

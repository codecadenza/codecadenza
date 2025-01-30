import { AbstractControl, ValidationErrors, ValidatorFn } from '@angular/forms';
import { NumberConverter } from '../converter/number-converter';

/**
 * Class that provides validators for integers and decimals
 */
export class NumberValidator {
  private static readonly INTEGER_REGEX = RegExp(/^-?\d+$/);

  /**
   * Create a validator that only allows decimal values
   */
  static decimal(numberConverter: NumberConverter): ValidatorFn {
    return (control: AbstractControl): { [key: string]: boolean } | null => {
      const value = control.value;

      try {
        if (!control.pristine && value && typeof value === 'string') {
          numberConverter.convertToNumber(value);
        }

        return null;
      } catch(error) {
        return { 'decimal': true };
      }
    };
  }

  /**
   * Create a validator that only allows integer values
   */
  static integer(control: AbstractControl): ValidationErrors | null {
   const value = control.value;

   if (!control.pristine && value && typeof value === 'string' && !NumberValidator.INTEGER_REGEX.test(value)) {
     return { 'integer': true };
   }

   return null;
  }

}

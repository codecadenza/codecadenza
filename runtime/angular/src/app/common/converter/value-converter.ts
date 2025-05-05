import { Injectable } from '@angular/core';
import { NumberConverter } from './number-converter';
import { FormatterService } from '../services/formatter.service';
import { ValueType } from './value-type.enum';

/**
 * Service for converting string values to the requested target type and vice versa
 */
@Injectable({ providedIn: 'root' })
export class ValueConverter {
  /**
   * Create a new instance
   */
  constructor(private readonly formatterService: FormatterService, private readonly numberConverter: NumberConverter) {
  }

  /**
   * Convert the given string to the actual type
   */
  convertToValue(value: any, valueType: ValueType): any {
    if (valueType === ValueType.DECIMAL || valueType === ValueType.INTEGER) {
      return this.numberConverter.convertToNumber(value);
    } else if (valueType === ValueType.DATE) {
      // Just check if date values can be formatted so that an exception is thrown if the value is invalid!
      this.formatterService.formatDate(value);
    } else if (valueType === ValueType.DATE_TIME) {
      this.formatterService.formatDateTime(value);
    }

    return value;
  }

  /**
   * Convert the value to a string
   */
  convertToString(value: string | Date | number | null, valueType: ValueType) {
    if (!value) {
      return '';
    }

    if (valueType === ValueType.DATE) {
      return this.formatterService.formatDate(value);
    } else if (valueType === ValueType.DATE_TIME) {
      return this.formatterService.formatDateTime(value);
    } else if (typeof value === 'number' && valueType === ValueType.DECIMAL) {
      return this.formatterService.formatNumber(value);
    }

    return value.toString();
  }

  /**
   * Create a string representation of a default value
   */
  getInitialDefaultValue(valueType: ValueType): string | null {
    if (valueType === ValueType.DATE) {
      // Simply return the ISO string, as there is no standard function available for parsing formatted dates!
      return new Date().toISOString().slice(0, 10);
    } else if (valueType === ValueType.DATE_TIME) {
      return new Date().toISOString()
    } else if (valueType === ValueType.DECIMAL) {
      return this.formatterService.formatNumber(0.1);
    }

    return '';
  }

}

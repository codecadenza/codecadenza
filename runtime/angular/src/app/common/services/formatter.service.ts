import { LOCALE_ID, Inject, Injectable } from '@angular/core';
import { DatePipe, DecimalPipe } from '@angular/common';
import { DateConverter } from '../converter/date-converter';
import { FieldTypeEnum } from '../model/field-type.enum';

/**
 * Service that is responsible for formatting date and numeric values
 * consistently in all parts of the application.
 */
@Injectable({ providedIn: 'root' })
export class FormatterService {
  private static readonly DEFAULT_DATE_TIME_FORMAT = 'dd.MM.yyyy HH:mm:ss';
  private static readonly DEFAULT_DATE_FORMAT = 'dd.MM.yyyy';
  private static readonly DEFAULT_NUMBER_FORMAT = '1.0-2';
  private static readonly STORAGE_ITEM_DATE_TIME_FORMAT = 'app-date-time-format';
  private static readonly STORAGE_ITEM_DATE_FORMAT = 'app-date-format';
  private static readonly STORAGE_ITEM_NUMBER_FORMAT = 'app-number-format';

  private readonly datePipe: DatePipe;
  private readonly decimalPipe: DecimalPipe;
  private dateFormatString = FormatterService.DEFAULT_DATE_FORMAT;
  private dateTimeFormatString = FormatterService.DEFAULT_DATE_TIME_FORMAT;
  private numberFormatString = FormatterService.DEFAULT_NUMBER_FORMAT;

  /**
   * Initialize internal properties
   */
  constructor(@Inject(LOCALE_ID) public locale: string) {
    const dateTimeFormat = localStorage.getItem(FormatterService.STORAGE_ITEM_DATE_TIME_FORMAT);
    const dateFormat = localStorage.getItem(FormatterService.STORAGE_ITEM_DATE_FORMAT);
    const numberFormat = localStorage.getItem(FormatterService.STORAGE_ITEM_NUMBER_FORMAT);

    this.datePipe = new DatePipe(locale);
    this.decimalPipe = new DecimalPipe(locale);

    if (dateTimeFormat) {
      this.dateTimeFormatString = dateTimeFormat;
    }

    if (dateFormat) {
      this.dateFormatString = dateFormat;
    }

    if (numberFormat) {
      this.numberFormatString = numberFormat;
    }
  }

  /**
   * Format the given date time value with the respective pattern
   */
  formatDateTime(value: string | Date | number | null) {
    if (!value) {
      return '';
    }

    return this.datePipe.transform(DateConverter.convertToDate(value), this.dateTimeFormatString);
  }

  /**
   * Format the given date value with the respective pattern
   */
  formatDate(value: string | Date | number | null) {
    if (!value) {
      return '';
    }

    return this.datePipe.transform(DateConverter.convertToDate(value), this.dateFormatString);
  }

  /**
   * Format the given numeric value with the respective pattern
   */
  formatNumber(value: string | number | null) {
    if (!value) {
      return '';
    }

    return this.decimalPipe.transform(value, this.numberFormatString);
  }

  /**
   * Format the value of either a numeric or date field displayed in a table cell
   */
  formatField(type: FieldTypeEnum, hasDateTimeFormat: boolean, value: unknown) {
    if (type === FieldTypeEnum.DATE && (typeof value === 'string' || typeof value === 'number' || value instanceof Date)) {
      if (hasDateTimeFormat) {
        return this.formatDateTime(value);
      } else {
        return this.formatDate(value);
      }
    } else if (type === FieldTypeEnum.DECIMAL && (typeof value === 'string' || typeof value === 'number')) {
      return this.formatNumber(value);
    }

    return value;
  }

  /**
   * Return the date time format pattern
   */
  getDateTimeFormat(): string {
    return this.dateTimeFormatString;
  }

  /**
   * Return the date format pattern
   */
  getDateFormat(): string {
    return this.dateFormatString;
  }

  /**
   * Return the number format pattern
   */
  getNumberFormat(): string {
    return this.numberFormatString;
  }

  /**
   * Set the date time format pattern and save it in the local storage
   */
  setDateTimeFormat(formatString: string) {
    this.dateTimeFormatString = formatString;
  }

  /**
   * Set the date format pattern and save it in the local storage
   */
  setDateFormat(formatString: string) {
    this.dateFormatString = formatString;
  }

  /**
   * Set the number format pattern and save it in the local storage
   */
  setNumberFormat(formatString: string) {
    this.numberFormatString = formatString;
  }

  /**
   * Get the locale of this service
   */
  getLocale() {
    return this.locale;
  }

  /**
   * Save the format settings in the local storage
   */
  saveFormatSettings() {
    if (this.dateTimeFormatString) {
      localStorage.setItem(FormatterService.STORAGE_ITEM_DATE_TIME_FORMAT, this.dateTimeFormatString);
    }

    if (this.dateFormatString) {
      localStorage.setItem(FormatterService.STORAGE_ITEM_DATE_FORMAT, this.dateFormatString);
    }

    if (this.numberFormatString) {
      localStorage.setItem(FormatterService.STORAGE_ITEM_NUMBER_FORMAT, this.numberFormatString);
    }
  }

}

/**
 * Converter for date values
 */
/* eslint-disable @typescript-eslint/no-explicit-any */
export class DateConverter {

  /**
   * Convert the given input to a date. This is necessary if the back-end provides date
   * values in a format which cannot be handled by the JSon parser.
   */
  static convertToDate(input: string | Date | number | null): any {
    if (!input) {
      return null;
    }

    // If the input is already a date a conversion won't be necessary!
    if (input instanceof Date) {
      return input;
    }

    if (typeof input === 'number') {
      const date: Date = new Date();
      date.setTime(Number.parseInt(input.toString(), 10));

      return date;
    }

    // Check if the time fragment is missing and add it if necessary
    if (input.length === 10) {
      return new Date(input + 'T00:00:00.000Z');
    }

    // Check if the value contains an opening bracket which indicates that the rest contains
    // the time zone that must be removed in order to convert the value properly!
    if (input.includes('[')) {
      return new Date(input.substring(0, input.indexOf('[')));
    }

    // Check if the milliseconds are missing. A value without milliseconds may cause severe
    // problems in different components!
    if (RegExp(/T\d{2}:\d{2}:\d{2}$/).test(input)) {
      return new Date(input + '.000Z');
    }

    return new Date(input);
  }

  /**
   * Remove the time fragment from the date
   */
  static removeTime(input: Date): any {
    if (!input) {
      return null;
    }

    const dateString = input.toISOString();

    return dateString.substring(0, dateString.indexOf('T'));
  }

  /**
   * Convert the given date into the time in milliseconds
   */
  static convertToMilliseconds(input: Date): any {
     if (!input) {
      return null;
    }

    return input.getTime();
  }

  /**
   * Remove the milliseconds from the ISO date representation
   */
  static removeMilliseconds(input: Date): any {
     if (!input) {
      return null;
    }

    const dateString = input.toISOString();

    return dateString.substring(0, dateString.lastIndexOf('.'));
  }

}

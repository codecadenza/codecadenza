/**
 * Abstract base class for data that is used for search operations
 */
export abstract class AbstractSearchInput {
  public static readonly DATE_FORMAT = 'yyyy-MM-dd';
  public static readonly DATE_TIME_FORMAT = 'yyyy-MM-dd\'T\'HH:mm:ss.sssZZ';

  maxResult: number;
  caseSensitive: boolean;
  numberFormat: string;
  dateFormat: string;
  startIndex: number;
  dateTimeFormat: string;
  exactFilterMatch: boolean;
  decimalSeparator: string;
  groupingSeparator: string;

  /**
   * Create a new instance of this class and initialize internal
   * fields with reasonable default values
   */
  constructor() {
    this.maxResult = 1000;
    this.caseSensitive = false;
    this.numberFormat = '0.00';
    this.dateFormat = AbstractSearchInput.DATE_FORMAT;
    this.dateTimeFormat = AbstractSearchInput.DATE_TIME_FORMAT;
    this.startIndex = 0;
    this.exactFilterMatch = true;
    this.decimalSeparator = '.';
    this.groupingSeparator = ',';
  }

}

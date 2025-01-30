import { Component, OnInit, Input, HostBinding } from '@angular/core';
import { UntypedFormGroup } from '@angular/forms';
import { I18NService } from '../../services/i18n.service';

/**
 * This component provides a responsive container for a given control.
 * Furthermore, the control's standard validators can be defined at a
 * central location by using respective attributes.
 */
@Component({
  selector: 'cc-form-control-container',
  templateUrl: './form-control-container.component.html'
})
export class FormControlContainerComponent implements OnInit {
  @HostBinding('class') parentClass!: string;
  @Input() label = '';
  @Input() name = '';
  @Input() formGroup!: UntypedFormGroup;
  @Input() fillEmptySpace = false;
  @Input() span = false;
  private _required = false;
  private _minLength = 0;
  private _maxLength = 0;
  private _maxValue = 0;
  private _minValue = 0;
  private _pattern = '';
  labelClass = '';
  requiredSet = false;
  minLengthSet = false;
  maxLengthSet = false;
  minValueSet = false;
  maxValueSet = false;
  patternSet = false;
  integerSet = false;
  decimalSet = false;
  messageRequired = '';
  messageMinLength = '';
  messageMaxLength = '';
  messageMinValue = '';
  messageMaxValue = '';
  messagePattern = '';
  messageInteger = '';
  messageDecimal = '';

  /**
   * Create a new instance
   */
  constructor(protected i18n: I18NService) {
  }

  /**
   * Initialize the component
   */
  ngOnInit() {
    this.parentClass = 'field col-12 md:col-6';

    if (this.span) {
      this.parentClass = 'field col-12';
    }

    if (this._required) {
      this.labelClass = 'label-field-mandatory';
      this.messageRequired = this.i18n.translate('validation_required');
    } else {
      this.labelClass = 'label-field-optional';
    }

    if (this.fillEmptySpace) {
      this.parentClass = 'field col-6';
    }

    if (this.minLengthSet) {
      this.messageMinLength = this.i18n.translate('validation_minlength', this._minLength.toString());
    }

    if (this.maxLengthSet) {
      this.messageMaxLength = this.i18n.translate('validation_maxlength', this._maxLength.toString());
    }

    if (this.minValueSet) {
      this.messageMinValue = this.i18n.translate('validation_minvalue', this._minValue.toString());
    }

    if (this.maxValueSet) {
      this.messageMaxValue = this.i18n.translate('validation_maxvalue', this._maxValue.toString());
    }

    if (this.patternSet) {
      this.messagePattern = this.i18n.translate('validation_pattern', this._pattern);
    }

    if (this.integerSet) {
      this.messageInteger = this.i18n.translate('validation_integer');
    }

    if (this.decimalSet) {
      this.messageDecimal = this.i18n.translate('validation_decimal');
    }
  }

  /**
   * Return true if an input is required
   */
  public get required(): boolean {
    return this._required;
  }

  /**
   * Set the required flag
   */
  @Input() public set required(value: boolean) {
    this.requiredSet = value;
    this._required = value;
  }

  /**
   * Return the minimum length to be entered
   */
  public get minLength(): number {
    return this._minLength;
  }

  /**
   * Set the minimum length
   */
  @Input() public set minLength(value: number) {
    this.minLengthSet = true;
    this._minLength = value;
  }

  /**
   * Return the maximum length to be entered
   */
  public get maxLength(): number {
    return this._maxLength;
  }

  /**
   * Set the maximum length
   */
  @Input() public set maxLength(value: number) {
    this.maxLengthSet = true;
    this._maxLength = value;
  }

  /**
   * Return the minimum value to be entered
   */
  public get minValue(): number {
    return this._minValue;
  }

  /**
   * Set the minimum value
   */
  @Input() public set minValue(value: number) {
    this.minValueSet = true;
    this._minValue = value;
  }

  /**
   * Return the maximum value to be entered
   */
  public get maxValue(): number {
    return this._maxValue;
  }

  /**
   * Set the maximum value
   */
  @Input() public set maxValue(value: number) {
    this.maxValueSet = true;
    this._maxValue = value;
  }

  /**
   * Return the pattern that the input must match
   */
  public get pattern(): string {
    return this._pattern;
  }

  /**
   * Set the pattern
   */
  @Input() public set pattern(value: string) {
    this.patternSet = true;
    this._pattern = value;
  }

  /**
   * Return true if the value must be an integer
   */
  public get integer(): boolean {
    return this.integerSet;
  }

  /**
   * Control if the integer validation should be enabled
   */
  @Input() public set integer(value: boolean) {
    this.integerSet = value;
  }

  /**
   * Return true if the value must be a decimal
   */
  public get decimal(): boolean {
    return this.decimalSet;
  }

  /**
   * Control if the decimal validation should be enabled
   */
  @Input() public set decimal(value: boolean) {
    this.decimalSet = value;
  }

  /**
   * Validate a control by using the given validator. The method will return true if the input is valid.
   */
  validateControl(validator: string): boolean {
    // Check if the form group has been defined
    if (!this.formGroup) {
      console.error('Cannot validate control "' + this.name + '" as no form group is defined!');
      return false;
    }

    const control = this.formGroup.controls[this.name];

    // Test if the control exists
    if (!control) {
      console.error('Cannot validate control "' + this.name + '" as it does not belong to this form group!');
      return false;
    }

    if (control.pristine) {
      return true;
    }

    return !control.hasError(validator);
  }

}

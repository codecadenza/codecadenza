import { Component, forwardRef, Input } from '@angular/core';
import { NG_VALUE_ACCESSOR, ControlValueAccessor } from '@angular/forms';

/**
 * Component for opening a page by using the provided data of a reactive form binding
 */
@Component({
  selector: 'cc-formlink',
  templateUrl: './formlink.component.html',
  providers: [{
    provide: NG_VALUE_ACCESSOR,
    useExisting: forwardRef(() => FormLink),
    multi: true
  }]
})
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export class FormLink<T extends { [key: string]: any; }> implements ControlValueAccessor {
  @Input() route = '';
  @Input() labelFieldName = '';
  @Input() idFieldName = '';
  value: T | null = null;

  /**
   * Write the value
   */
  writeValue(value: T) {
    if (value !== this.value) {
      this.value = value;
    }
  }

  /**
   * Callback function that is called when the control's value changes in the UI
   */
  registerOnChange() {
    // No implementation required!
  }

  /**
   * Callback function that is called by the forms API on initialization to update the form model on blur
   */
  registerOnTouched() {
    // No implementation required!
  }

  /**
   * Return the destination address of the link
   */
  getAddress(): string {
    if (!this.route || !this.value) {
      return '';
    }

    const idValue = this.value[this.idFieldName];

    return this.route + '/' + idValue;
  }

  /**
   * Return the text that should be displayed
   */
  getLinkText(): string {
    if (!this.value) {
      return '';
    }

    const linkText = this.value[this.labelFieldName];

    if (typeof linkText === 'string') {
      return linkText;
    } else if (typeof linkText === 'number') {
      return linkText.toString();
    }

    throw Error('Unsupported type for label field: "' + this.labelFieldName + '"');
  }

}

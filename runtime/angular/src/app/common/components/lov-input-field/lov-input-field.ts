import { Component, forwardRef, Input, EventEmitter, Output } from '@angular/core';
import { NG_VALUE_ACCESSOR, ControlValueAccessor } from '@angular/forms';
import { Bind } from 'primeng/bind';
import { InputText } from 'primeng/inputtext';
import { Button } from 'primeng/button';

/**
 * Input field for list-of-values
 */
@Component({
  selector: 'cc-lov-input-field',
  templateUrl: './lov-input-field.html',
  providers: [{
    provide: NG_VALUE_ACCESSOR,
    useExisting: forwardRef(() => LovInputField),
    multi: true
  }],
  imports: [Bind, InputText, Button]
})
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export class LovInputField<T extends Record<string, any>> implements ControlValueAccessor {
  @Input() public labelFieldName = '';
  @Input() public readonly = false;
  @Output() protected searchClicked = new EventEmitter();
  protected value: T | null = null;

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
   * Inform the parent component that the search button has been clicked
   */
  onSearchClicked() {
    this.searchClicked.emit();
  }

  /**
   * Return the text that should be displayed for an item
   */
  getItemText(): string {
    if (!this.value) {
      return '';
    }

    const labelFieldValue = this.value[this.labelFieldName];

    if (typeof labelFieldValue === 'string') {
      return labelFieldValue;
    } else if (typeof labelFieldValue === 'number') {
      return labelFieldValue.toString();
    }

    throw Error('Unsupported type for label field: "' + this.labelFieldName + '"');
  }

}

import { Component, forwardRef } from '@angular/core';
import { NG_VALUE_ACCESSOR, ControlValueAccessor } from '@angular/forms';

/**
 * Component that supports the binding of a reactive form field to the 'href' attribute of a HTML hyperlink
 */
@Component({
  selector: 'cc-weblink',
  templateUrl: './weblink.component.html',
  providers: [{
    provide: NG_VALUE_ACCESSOR,
    useExisting: forwardRef(() => WebLink),
    multi: true
  }]
})
export class WebLink implements ControlValueAccessor {
  value = '';

  /**
   * Write the value
   */
  writeValue(value: string) {
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

}

import { Directive, ElementRef, HostListener, inject, OnInit } from '@angular/core';
import { FormatterService } from '../services/formatter.service';

/**
 * Directive for formatting date values
 */
@Directive({ selector: '[ccDateFormatter]' })
export class DateFormatterDirective implements OnInit {
  private readonly formatter = inject(FormatterService);
  private readonly elementRef = inject(ElementRef);
  private readonly el = this.elementRef.nativeElement;

  /**
   * Format the value after the component has been initialized
   */
  public ngOnInit() {
    this.formatValue();
  }

  /**
   * Format the value after a model-change event
   */
  @HostListener('ngModelChange')
  public onModelChange() {
    this.formatValue();
  }

  private formatValue() {
    if (!this.el.value) {
      return;
    }

    let valueToConvert: string | number;

    if (isNaN(Number(this.el.value))) {
      valueToConvert = this.el.value;
    } else {
      // Convert the value to a number so that it can be formatted properly!
      valueToConvert = Number(this.el.value);
    }

    const convertedValue = this.formatter.formatDate(valueToConvert);

    if (!convertedValue) {
      this.el.value = '';
      return;
    }

    this.el.value = convertedValue;
  }

}

import { Directive, ElementRef, inject, OnInit } from '@angular/core';
import { FormatterService } from '../services/formatter.service';

/**
 * Directive for formatting numbers
 */
@Directive({ selector: '[ccNumberFormatter]' })
export class NumberFormatterDirective implements OnInit {
  private readonly formatter = inject(FormatterService);
  private readonly elementRef = inject(ElementRef);
  private readonly el = this.elementRef.nativeElement;

  /**
   * Format the value after the component has been initialized
   */
  public ngOnInit() {
    const convertedValue = this.formatter.formatNumber(this.el.value);

    if (!convertedValue) {
      this.el.value = '';
      return;
    }

    this.el.value = convertedValue;
  }

}

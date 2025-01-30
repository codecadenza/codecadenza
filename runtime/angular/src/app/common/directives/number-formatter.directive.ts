import { Directive, ElementRef, OnInit } from '@angular/core';
import { FormatterService } from '../services/formatter.service';

/**
 * Directive for formatting numbers
 */
@Directive({ selector: '[ccNumberFormatter]' })
export class NumberFormatterDirective implements OnInit {
  private readonly el: HTMLInputElement;

  /**
   * Initialize the directive
   */
  constructor(protected formatter: FormatterService, private readonly elementRef: ElementRef) {
    this.el = this.elementRef.nativeElement;
  }

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

import { Component, HostBinding, OnInit } from '@angular/core';

/**
 * This component represents a responsive container for form buttons
 */
@Component({
  selector: 'cc-form-button-container',
  templateUrl: './form-button-container.component.html'
})
export class FormButtonContainerComponent implements OnInit {
  @HostBinding('style.padding') parentPadding!: number;

  /**
   * Initialize the style of the parent element
   */
  ngOnInit() {
    this.parentPadding = 0;
  }

}

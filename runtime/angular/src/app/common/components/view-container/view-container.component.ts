import { Component, Input, ViewEncapsulation } from '@angular/core';

/**
 * Container component for all pages of this application
 */
@Component({
  selector: 'cc-view-container',
  encapsulation: ViewEncapsulation.None,
  templateUrl: './view-container.component.html',
  styleUrls: ['view-container.component.css']
})
export class ViewContainer {
  @Input() headerText = '';
  @Input() headerIcon?: string;
  @Input() contentPadding = '0.5rem';
  @Input() enableCollapse = false;
  @Input() collapsed = false;

}

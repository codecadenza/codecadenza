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
  @Input() public headerText = '';
  @Input() public headerIcon?: string;
  @Input() public contentPadding = '0.5rem';
  @Input() public enableCollapse = false;
  @Input() public collapsed = false;

}

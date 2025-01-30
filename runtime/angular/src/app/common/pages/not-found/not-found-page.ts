import { Component } from '@angular/core';
import { Router } from '@angular/router';

/**
 * Page that is displayed if the requested resource could not be found
 */
@Component({
  templateUrl: './not-found-page.html'
})
export class NotFoundPage {
  visible = true;

  /**
   * Create a new instance
   */
  constructor(protected router: Router) {
  }

  /**
   * Navigate to the start page if the dialog is closed
   */
  onCloseDialog() {
    this.router.navigate(['/']);
  }

}

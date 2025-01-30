import { Router, NavigationEnd } from '@angular/router';
import { Injectable } from '@angular/core';

/**
 * Service that keeps track of the navigation history and provides a browser independent
 * function to navigate to the previous page. This is necessary as using 'location.back()'
 * doesn't work as expected in some browsers (e.g. Chrome)!
 */
@Injectable({ providedIn: 'root' })
export class NavigationHistoryService {
  private readonly history: string[] = [];

  /**
   * Create a new instance
   */
  constructor(protected router: Router) {
  }

  /**
   * Start tracking the navigation history
   */
  public startTracking() {
    this.router.events.subscribe(
      event => {
        if (event instanceof NavigationEnd) {
          this.history.push(event.url);
        }
      });
  }

  /**
   * Navigate back to the previous page
   */
  public navigateToPreviousPage() {
    if (this.history.length < 2) {
      return;
    }

    // Skip the last element as it represents the URL of the current page!
    this.history.pop();

    // The next element represents the previous URL
    this.router.navigate([this.history.pop()]);
  }

}

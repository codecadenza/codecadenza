import { Injectable, NgZone } from '@angular/core';
import { ViewportRuler } from '@angular/cdk/scrolling';
import { Observable, BehaviorSubject } from 'rxjs';

/**
 * Service that keeps track of the current window size and emits respective events
 */
@Injectable({ providedIn: 'root' })
export class WindowResizeEventController {
  private readonly WIDTH_THRESHOLD = 1024;
  private hasSmallWidth = false;
  private readonly widthThresholdSubject$ = new BehaviorSubject<boolean>(false);

  /**
   * Create a new instance
   */
  constructor(private readonly viewportRuler: ViewportRuler, private readonly ngZone: NgZone) {
    this.viewportRuler.change(200).subscribe(() => this.ngZone.run(() => this.checkWindowWidth()));
    this.checkWindowWidth();
  }

  /**
   * Inform all listeners that the window width is either larger or smaller than 1024 pixels
   */
  public onWidthThreshold(): Observable<boolean> {
    return this.widthThresholdSubject$.asObservable();
  }

  /**
   * Determine the current window width and check if a respective event should be emitted
   */
  private checkWindowWidth() {
    const width = this.viewportRuler.getViewportSize().width;
    const previousStatus = this.hasSmallWidth;

    if (width < this.WIDTH_THRESHOLD) {
      this.hasSmallWidth = true;
    } else {
      this.hasSmallWidth = false;
    }

    // Emit an event if the status has changed!
    if (previousStatus !== this.hasSmallWidth) {
      this.widthThresholdSubject$.next(this.hasSmallWidth);
    }
  }

}

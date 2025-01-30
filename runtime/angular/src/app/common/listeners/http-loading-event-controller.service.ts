import { Injectable } from '@angular/core';
import { Observable, BehaviorSubject } from 'rxjs';

/**
 * Service for emitting and listening to HTTP loading events
 */
@Injectable({ providedIn: 'root' })
export class HTTPLoadingEventController {
  private readonly loadingSubject$: BehaviorSubject<boolean>;

  /**
   * Create a new subject
   */
  constructor() {
    this.loadingSubject$ = new BehaviorSubject<boolean>(false);
  }

  /**
   * Change the loading status
   */
  setLoading(value: boolean) {
    this.loadingSubject$.next(value);
  }

  /**
   * Inform all listeners that the loading status has changed
   */
  onLoadingStatusChanged(): Observable<boolean> {
    return this.loadingSubject$.asObservable();
  }

}

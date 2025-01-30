import { Injectable } from '@angular/core';
import { HttpRequest, HttpHandler, HttpEvent, HttpInterceptor } from '@angular/common/http';
import { Observable } from 'rxjs';

/**
 * HTTP interceptor for adding basic authentication credentials
 */
@Injectable()
export class HTTPBasicAuthInterceptor implements HttpInterceptor {

  /**
   * Intercept all HTTP requests
   */
  intercept(request: HttpRequest<undefined>, next: HttpHandler): Observable<HttpEvent<undefined>> {
    // Load the basic authentication credentials from the session storage
    const credentials = sessionStorage.getItem('credentials');

    if (credentials) {
      // Add the credentials to the HTTP header
      request = request.clone({ setHeaders: { Authorization: 'Basic ' + credentials } });
    }

    return next.handle(request);
  }

}

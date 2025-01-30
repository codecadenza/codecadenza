import { Injectable } from '@angular/core';
import { HttpRequest, HttpHandler, HttpEvent, HttpInterceptor } from '@angular/common/http';
import { Observable } from 'rxjs';

/**
 * HTTP interceptor for adding authentication credentials
 */
@Injectable()
export class HTTPAuthInterceptor implements HttpInterceptor {

  /**
   * Intercept all HTTP requests
   */
  intercept(request: HttpRequest<undefined>, next: HttpHandler): Observable<HttpEvent<undefined>> {
    // Load the user name and the password from the session storage
    const userName = sessionStorage.getItem('username');
    const password = sessionStorage.getItem('password');

    if (userName && password) {
      // Add the credentials to the HTTP header
      request = request.clone({ setHeaders: { username: userName, password: password } });
    }

    return next.handle(request);
  }

}

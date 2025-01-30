import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable, of } from 'rxjs';
import { RoleEnum } from '../model/role.enum';

/**
 * Service that provides authentication and authorization features.
 * Most of the methods don't provide a respective functionality. Thus,
 * the methods must be implemented either manually or the project's
 * security configuration must be available to generate respective source code!
 */
@Injectable({ providedIn: 'root' })
export class AuthService {
  private readonly loginStatusSubject: BehaviorSubject<boolean>;

  /**
   * Create a new instance of this service and initialize the login status subject
   */
  constructor() {
    this.loginStatusSubject = new BehaviorSubject<boolean>(this.isLoggedIn());
  }

  /**
   * Check if the user has at least one of the given roles granted
   */
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  hasPermission(_roles: RoleEnum[]): boolean {
    // Not implemented yet!
    return true;
  }

  /**
   * Return true if the user has logged in successfully
   */
  isLoggedIn(): boolean {
    // Not implemented yet!
    return true;
  }

  /**
   * Notify all listeners as soon as the login status has changed
   */
  onLoginStatusChanged(): Observable<boolean> {
    return this.loginStatusSubject.asObservable();
  }

  /**
   * Perform the login operation
   */
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  login(_userName: string, _password: string): Observable<void> {
    // Not implemented yet!
    return of();
  }

  /**
   * Perform the logout operation
   */
  logout() {
    // Not implemented yet!
  }

  /**
   * Change the password
   */
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  changePassword(_oldPassword: string, _newPassword: string, _confirmedPassword: string): Observable<void> {
    // Not implemented yet!
    return of();
  }

}

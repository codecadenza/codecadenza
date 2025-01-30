import { Injectable, inject } from '@angular/core';
import { Router, ActivatedRouteSnapshot, CanActivateFn } from '@angular/router';
import { AuthService } from './auth.service';

/**
 * Service that prevents access to protected resources if the user is either
 * not logged in or he lacks the necessary permissions
 */
@Injectable({ providedIn: 'root' })
export class AuthGuardService {

  /**
   * Create a new instance
   */
  constructor(protected authService: AuthService, protected router: Router) {
  }

  /**
   * Return true if the resource may be accessed
   */
  canActivate(route: ActivatedRouteSnapshot): boolean {
    // Validate if the user is logged in
    if (!this.authService.isLoggedIn()) {
      this.router.navigate(['/login']);
      return false;
    }

    const skipPermissionCheck = route.data['skipPermissionCheck'];

    if (skipPermissionCheck) {
      return true;
    }

    const roles = route.data['roles'];

    // Check permissions
    if (!roles || !this.authService.hasPermission(roles)) {
      console.error('Permission denied for resource ' + route.component + '!');
      return false;
    }

    return true;
  }

}

export const AuthGuard: CanActivateFn = (next: ActivatedRouteSnapshot): boolean => inject(AuthGuardService).canActivate(next);

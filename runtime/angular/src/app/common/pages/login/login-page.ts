import { Component, OnInit } from '@angular/core';
import { UntypedFormGroup, UntypedFormControl, Validators } from '@angular/forms';
import { AuthService } from '../../services/auth.service';

/**
 * Login page
 */
@Component({
  templateUrl: './login-page.html'
})
export class LoginPage implements OnInit {
  formGroup!: UntypedFormGroup;
  credentials = { userName: '', password: '' };

  /**
   * Create a new instance
   */
  constructor(protected authService: AuthService) {
  }

  /**
   * Initialize the form
   */
  ngOnInit() {
    this.formGroup = new UntypedFormGroup({
      userName: new UntypedFormControl('', Validators.required),
      password: new UntypedFormControl('', Validators.required)
    });

    this.formGroup.patchValue(this.credentials);
  }

  /**
   * Perform the log in operation
   */
  performLogin() {
    Object.assign(this.credentials, this.formGroup.value);

    this.authService.login(this.credentials.userName, this.credentials.password);
  }

}

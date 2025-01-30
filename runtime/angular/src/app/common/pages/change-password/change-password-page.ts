import { Component, OnInit } from '@angular/core';
import { Validators, FormGroup, FormControl } from '@angular/forms';
import { AuthService } from '../../services/auth.service';
import { ChangePassword } from '../../model/change-password.model';

/**
 * Page for changing the user's password
 */
@Component({
  templateUrl: './change-password-page.html'
})
export class ChangePasswordPage implements OnInit {
  formGroup!: FormGroup;
  passwords!: ChangePassword;

  /**
   * Create a new instance
   */
  constructor(protected authService: AuthService) {
  }

  /**
   * Initialize the form
   */
  ngOnInit() {
    this.formGroup = new FormGroup({
      oldPassword: new FormControl('', Validators.required),
      newPassword: new FormControl('', Validators.required),
      confirmedPassword: new FormControl('', Validators.required)
    });

    this.passwords = {
      oldPassword: '',
      newPassword: '',
      confirmedPassword: ''
    };

    this.formGroup.patchValue(this.passwords);
  }

  /**
   * Save the new password
   */
  savePassword() {
    Object.assign(this.passwords, this.formGroup.value);

    this.authService.changePassword(this.passwords.oldPassword, this.passwords.newPassword, this.passwords.confirmedPassword);
  }

}

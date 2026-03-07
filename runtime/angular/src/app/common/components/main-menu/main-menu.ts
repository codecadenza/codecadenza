import { Component, inject, OnInit } from '@angular/core';
import { MenubarModule } from 'primeng/menubar';
import { ButtonModule } from 'primeng/button';
import { MenuItem } from 'primeng/api';
import { CommonModule } from '@angular/common';
import { AuthService } from '../../services/auth.service';
import { I18NService } from '../../services/i18n.service';

/**
 * Component that contains the main menu
 */
@Component({
  selector: 'app-main-menu',
  imports: [MenubarModule, ButtonModule, CommonModule],
  templateUrl: './main-menu.html'
})
export class MainMenu implements OnInit {
  protected readonly authService = inject(AuthService);
  protected readonly i18n = inject(I18NService);
  protected items: MenuItem[] | undefined;

  /**
   * Initialize the menu items
   */
  public ngOnInit() {
    this.items = [
      {
        label: this.i18n.translate('action_home'),
        icon: 'pi pi-home',
        routerLink: '/welcome'
      },
      {
        label: this.i18n.translate('action_settings'),
        icon: 'pi pi-cog',
        routerLink: '/settings'
      },
      {
        label: this.i18n.translate('action_change_password'),
        icon: 'pi pi-key',
        routerLink: '/changepassword'
      }
    ];
  }

  /**
   * Logout the user
   */
  protected onLogout() {
    this.authService.logout();
  }
}
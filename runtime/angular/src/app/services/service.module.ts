import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { HttpClientModule, HttpClient } from '@angular/common/http';

/**
 * Module for all services of this application
 */
@NgModule({
  imports: [
    CommonModule, HttpClientModule
  ],
  providers: [HttpClient],
  declarations: []
})
export class ServiceModule {}

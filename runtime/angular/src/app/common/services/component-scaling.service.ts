import { Inject, Injectable, Renderer2, RendererFactory2 } from '@angular/core';
import { DOCUMENT } from '@angular/common';

/**
 * Service that is responsible for the global component scaling
 */
@Injectable({ providedIn: 'root' })
export class ComponentScalingService {
  public static readonly MIN_FONT_SIZE = 8;
  public static readonly MAX_FONT_SIZE = 18;
  private fontSize = 13;
  private readonly renderer: Renderer2;

  /**
   * Create a new instance and try to load the global font size from the local storage
   */
  constructor(@Inject(DOCUMENT) private readonly document: Document,
    private readonly rendererFactory: RendererFactory2) {
    const storedFontSize = localStorage.getItem('app-font-size');
    this.renderer = rendererFactory.createRenderer(null, null);

    if (!storedFontSize || Number.isNaN(parseInt(storedFontSize, 10))) {
      return;
    }

    const appFontSize = parseInt(storedFontSize, 10);

    if (!this.isFontSizeInRange(appFontSize)) {
      return;
    }

    this.fontSize = appFontSize;
  }

  /**
   * Initialize the component scaling by using the global font size loaded from the local storage
   */
  initComponentScaling() {
    this.changeComponentScaling(this.fontSize);
  }

  /**
   * Return the global font size
   */
  getFontSize(): number {
    return this.fontSize;
  }

  /**
   * Change the component scaling by changing the global font size
   */
  changeComponentScaling(fontSize: number) {
    if (!this.isFontSizeInRange(fontSize)) {
      return;
    }

    this.fontSize = fontSize;

    this.renderer.setStyle(this.document.querySelector(':root'), 'font-size', this.fontSize + 'px');

    localStorage.setItem('app-font-size', fontSize.toString());
  }

  /**
   * Check if the provided font size is within the valid range
   */
  isFontSizeInRange(fontSize: number): boolean {
    if (fontSize < ComponentScalingService.MIN_FONT_SIZE || fontSize > ComponentScalingService.MAX_FONT_SIZE) {
      return false;
    }

    return true;
  }

}

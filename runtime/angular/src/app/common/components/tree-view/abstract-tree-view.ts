import { Component, inject, signal } from '@angular/core';
import { Router } from '@angular/router';
import { ConfirmationService, TreeNode, MessageService, MenuItem } from 'primeng/api';
import { FormatterService } from '../../services/formatter.service';
import { I18NService } from '../../services/i18n.service';

/**
 * Abstract base class for all tree views
 */
@Component({
  template: ''
})
export abstract class AbstractTreeView {
  protected readonly router = inject(Router);
  protected readonly confirmationService = inject(ConfirmationService);
  protected readonly messageService = inject(MessageService);
  protected readonly formatterService = inject(FormatterService);
  protected readonly i18n = inject(I18NService);
  protected readonly statusText = signal('');
  protected readonly selectedNode = signal<TreeNode | TreeNode[] | null>(null);
  protected readonly rootNodes = signal<TreeNode[]>([]);

  /**
   * Open a confirmation dialog before calling the provided delete function
   */
  openConfirmDeleteDialog(deleteFunction: () => void) {
    if (!this.selectedNode) {
      return;
    }

    this.confirmationService.confirm({
      message: this.i18n.translate('msg_confirmdelete'),
      header: this.i18n.translate('dlg_header_conf'),
      icon: 'pi pi-exclamation-triangle',
      accept: () => deleteFunction()
    });
  }

  /**
   * Remove the given node from the tree view
   */
  removeNode(nodeToRemove: TreeNode | TreeNode[] | null) {
    if (!nodeToRemove || Array.isArray(nodeToRemove)) {
      return;
    }

    this.rootNodes.update(nodes => {
      const updatedNodes = this.removeRecursive(nodes, nodeToRemove);
      this.displayNumberOfItemsInStatusField(updatedNodes.length);
      return updatedNodes;
    });
  }

  removeRecursive(nodes: TreeNode[], nodeToRemove: TreeNode): TreeNode[] {
    return nodes.filter(node => node.key !== nodeToRemove.key)
      .map(node => ({
        ...node,
        children: node.children ? this.removeRecursive(node.children, nodeToRemove) : []
      }));
  }

  /**
   * Replace or add children to a specific node
   */
  updateNodeChildren(nodeToUpdate: TreeNode, children: TreeNode[]) {
    this.rootNodes.update(nodes => this.updateRecursive(nodes, nodeToUpdate, children));
  }

  updateRecursive(nodes: TreeNode[], nodeToUpdate: TreeNode, newChildren: TreeNode[]): TreeNode[] {
    return nodes.map(node => {
      if (node.key === nodeToUpdate.key) {
        return { ...node, children: newChildren };
      }

      if (node.children) {
        return { ...node, children: this.updateRecursive(node.children, nodeToUpdate, newChildren) };
      }

      return node;
    });
  }

  /**
   * Add a new context menu item to the given array
   */
  addContextMenuItem(items: MenuItem[], label: string, icon: string, command: () => void) {
    const menuItem = { label: label, icon: icon, command: command };

    items.push(menuItem);
  }

  /**
   * Display the given number of items in the status text field
   */
  displayNumberOfItemsInStatusField(numberOfItems: number) {
    this.statusText.set(this.i18n.translate('msg_treeviewstatustext', numberOfItems.toString()));
  }

  /**
   * Display a message in the status text field that indicates that the application is currently loading data
   */
  displayLoadingMessageInStatusField() {
    this.statusText.set(this.i18n.translate('msg_statusloading'));
  }

  /**
   * Create an error message and display the error in the status text field
   */
  displayError(error: Error, errorMsg: string) {
    console.log(error);

    this.messageService.add({ severity: 'error', summary: errorMsg });
    this.statusText.set(errorMsg);
  }

}

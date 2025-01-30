import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { ConfirmationService, TreeNode, MessageService, MenuItem } from 'primeng/api';
import { FormatterService } from '../../services/formatter.service';
import { I18NService } from '../../services/i18n.service';

/**
 * Abstract base class for all tree views
 */
@Component({ template: ''})
export abstract class AbstractTreeView {
  statusText = '';
  selectedNode: TreeNode | TreeNode[] | null = null;
  rootNodes: TreeNode[] = [];

  /**
   * Create a new instance
   */
  constructor(protected messageService: MessageService, protected confirmationService: ConfirmationService,
    protected router: Router, protected formatterService: FormatterService, protected i18n: I18NService) {
  }

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
  removeNode(treeNode: TreeNode | TreeNode[] | null) {
    if (!treeNode || Array.isArray(treeNode)) {
      return;
    }

    const parentNode = treeNode.parent;
    let nodes: TreeNode[] | undefined;

    if (!this.selectedNode || Array.isArray(this.selectedNode)) {
      return;
    }

    if (parentNode) {
      nodes = parentNode.children;
    } else {
      nodes = this.rootNodes;
    }

    if (!nodes) {
      return;
    }

    for (let i = 0; i < nodes.length; i++) {
      if (nodes[i].data === this.selectedNode.data) {
        nodes.splice(i, 1);
        break;
      }
    }

    if (!parentNode) {
      this.displayNumberOfItemsInStatusField(nodes.length);
    }
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
    this.statusText = this.i18n.translate('msg_treeviewstatustext', numberOfItems.toString());
  }

  /**
   * Display a message in the status text field that indicates that the application is currently loading data
   */
  displayLoadingMessageInStatusField() {
    this.statusText = this.i18n.translate('msg_statusloading');
  }

  /**
   * Create an error message and display the error in the status text field
   */
  displayError(error: Error, errorMsg: string) {
    console.log(error);

    this.messageService.add({ severity: 'error', summary: errorMsg });
    this.statusText = errorMsg;
  }

}
